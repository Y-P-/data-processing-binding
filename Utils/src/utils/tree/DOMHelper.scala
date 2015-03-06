package utils.tree

import scala.collection.GenTraversableOnce
import org.w3c.dom.{Document,Element,Node,NodeList,NamedNodeMap,Attr,Text}
import javax.xml.XMLConstants

/** Contains some utilities for building DOM trees in a specific way.
 *  These are of course used by DOMPrefixTree, but they don't depend in any way on the PrefixTree class.
 */
object DOMHelper {

  def getData[V](elt:Node):Option[V] =
    elt.getUserData("").asInstanceOf[Option[V]]
  def setData[V](elt:Node,v:Option[V]):Unit =
    elt.setUserData("",v,null)

  def getParams[P](elt:Node):P   = {
    val p = elt.getOwnerDocument.getUserData("")
    if (p==null) throw new IllegalArgumentException("The owner document is not a valid document for a DOMPrefixTree")
    p.asInstanceOf[P]
  }

  protected class NodeListIterator(val nd:NodeList) extends Iterator[Node] {
    if (nd==null) throw new IllegalArgumentException
    private var i = 0
    def hasNext: Boolean = i<nd.getLength
    def next: Node = { val r=nd.item(i); i+=1; r }
  }
  object NodeListIterator {
    implicit def apply(nd:NodeList) = if (nd==null) Iterator.empty else new NodeListIterator(nd)
  }
  protected class AttrListIterator(val nd:NamedNodeMap) extends Iterator[Attr] {
    if (nd==null) throw new IllegalArgumentException
    private var i = 0
    def hasNext: Boolean = i<nd.getLength
    def next: Attr = { val r=nd.item(i).asInstanceOf[Attr]; i+=1; r }
  }
  object AttrListIterator {
    implicit def apply(nd:NamedNodeMap) = if (nd==null) Iterator.empty else new AttrListIterator(nd)
  }

  trait DOMContainer {
    //embedded node
    def elt:Node
  }

  /** Some data and methods required to properly build the DOM.
   *  It contains many settings.
   *  - noDefault is from the parent
   *  - stripEmpty is from the parent
   *  - doc is the document used for the DOM ; it should change for each tree, and this prevents any implicit default
   *    it is modified to contain this Params value as user data so as to share it between all children instances
   *  - topTag is the tag to be used for the top node
   *  - toText is a method to convert V to a text value ; if null the value is not output as text
   *  - valueTag is the name of the tag for the value ; if null or "", the value is output as a text node that comes last
   *    otherwise is output either as an Element containing a single TextNode or an attribute, depending on whether the tag starts with @.
   *  - toXMLname is a method that determines how to map an input String name as a valid xml tag (all strings are not acceptable for xml.)
   *    in addition, if the result starts with @, it will be created as an attribute (whose name matches the end of the string) if that
   *    is possible. If this is null, it is considered as identity.
   */
  trait Params[-V,T<:DOMContainer] {
    //the container document ; only one per tree
    def doc:Document
    //the top tag for the top element
    def topTag:String
    //converting V to String for the XML text value ; can be null
    def toText:V=>String
    //the tag for the value container
    def valueTag:String
    //the conversion from external name to XML name ; prefixes must be declared in namespaces
    def toXMLname:String=>String
    //the default namespace ; can be null
    def defaultNamespace:String
    //known namespaces (ns,prefix) ; the prefixes can be used in toXMLname
    def namespaces:Seq[(String,String)]
    //remove empty nodes ?
    def stripEmpty:Boolean

    //convert a node to T
    def toT(nd:Node):T

    //associates this Param with the doc
    doc.setUserData("", this, null)
    val hasNS = !namespaces.isEmpty

    val vTag = analyzeTag(valueTag)
    val tTag = analyzeTag(topTag)

    def isTreeNode(nd:Node) = nd match {
      case t:Text => false                                       //no Text node match a tree Node
      case a:Attr => if (a.getNodeName.startsWith("xml")) false  //special attribute cannot match (restricted name)
                     else if (vTag._1) !(nd.getLocalName==vTag._3 && nd.getNamespaceURI==vTag._2) //must not match text attribute
                     else true
      case e:Element => vTag==null || vTag._1 || !(nd.getLocalName==vTag._3 && nd.getNamespaceURI==vTag._2) //must not match text node
    }

    //return true if the node is an user node (part of the real tree, not added for DOM reasons)
    val filter: Node=>Boolean = nd => isTreeNode(nd)

    //translates the key and analyzes the result
    def analyzeKey(key:String) =
      analyzeTag(if (toXMLname==null) key else toXMLname(key))

    //analyzes a tag (format [@][prefix:]name and returns a quadruplet (isAttribute,namespace,name,localname) ; null if the tag is ignored
    def analyzeTag(tag:String) = {
      if (tag==null)
        null
      else {
        val idx    = tag.lastIndexOf(":")
        val isAttr = tag.charAt(0)=='@'
        val prefix = idx match {
          case -1 => null
          case  n => tag.substring(if (isAttr) 1 else 0,n)
        }
        val ns = prefix match {
          case null => defaultNamespace
          case p    => namespaces.find(_._2==p) match {
            case None        => throw new IllegalArgumentException(s"prefix '$prefix' has no known associated namespace for tag $tag")
            case Some((x,_)) => x
          }
        }
        val name = tag.substring(if (isAttr) 1 else 0 ) //the prefixed xml name
        val localname = tag.substring(idx+1) //the local name
        (isAttr,ns,name,localname)
      }
    }

    //creates the appropriate text representation
    def mkTxtNode(elt:Element,txt:String):Unit = {
      if (vTag==null) {                    //case 1: V text is a text node
        elt.appendChild(doc.createTextNode(txt))
      } else if (vTag._1) {                    //case 2: V text is an attribute
        val nd = doc.createAttributeNS(vTag._2,vTag._3)
        nd.setValue(txt)
        elt.setAttributeNode(nd)
      } else {                                 //case 3: V text is a text node inside a standard element node
        val nd = doc.createElementNS(vTag._2,vTag._3) //build container element
        nd.appendChild(doc.createTextNode(txt))
        elt.appendChild(nd)
      }
    }

    //creates a top node
    def mkTopNode(v:Option[V]):Element = {
      val nd = doc.createElementNS(tTag._2,tTag._3)
      setData(nd,v)
      if (defaultNamespace!=null)
        nd.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns", defaultNamespace)
      if (hasNS) for (x <- namespaces)
        nd.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns:" + x._2, x._1)
      if (v!=null && v!=None && toText!=null)
        mkTxtNode(nd,toText(v.get))
      nd
    }

    //top adopt node with new key
    def adoptNode(parent:Element, elt:Node, key:String):Unit = {
      val ak = analyzeKey(key)
      if (ak!=null) {
        val nd0 = if (ak._1) {
          if (hasChildren(elt))
            throw new IllegalStateException(s"a non empty Node cannot be transformed as an attribute: key=$key")
          val nd = doc.createAttributeNS(ak._2,ak._3)
          nd.setUserData("", getData[V](elt), null)
          val txt = getTextValue(elt)
          if (txt!=null)
            nd.setValue(txt)
          parent.setAttributeNodeNS(nd)
          nd
        } else {
          val nd = doc.renameNode(elt, ak._2, ak._3)
          parent.appendChild(nd)
          nd
        }
        if (key!=ak._3)
          nd0.setUserData("$", key, null)
      }
    }

    //returns the triplet (isAttribute,namespace,local XML name) for a given string ; eliminates matches with the valueTag (conflict)
    def xmlName(s:String):(Boolean,String,String) = {
      val r= (if (toXMLname==null) s else toXMLname(s)) match {
        case t if t==valueTag => null
        case t                => t
      }
      val r1 = if (r!=null && r.charAt(0)=='@') (true,r.substring(1)) else (false,r)
      r1._2.lastIndexOf(":") match {
        case -1 => (r1._1,defaultNamespace,r1._2)
        case n  => val ns = r1._2.substring(0,n-1)
                   val x = namespaces.find(_._1==ns)
                   if (x==None) throw new IllegalArgumentException(s"Namespace prefix $ns not found")
                   (r1._1,x.get._2,r1._2)
      }
    }

    //iterator on the "user" children of the Node ; includes both attributes and elements if appropriate
    def iterator(nd:Node): Iterator[(String,T)] = {
      val it:Iterator[Node] = (AttrListIterator(nd.getAttributes)++NodeListIterator(nd.getChildNodes)).filter(filter)
      it.map { e=>
        val name = e.getUserData("$") match {
          case null     => e.getNodeName
          case s:String => s
        }
        (name,toT(e))
      }
    }

    //lists the "user" children of the Node having the given key ; includes both attributes and elements if appropriate
    def findAll(nd:Node, key:String): Seq[Node] = {
      val ak = analyzeKey(key)
      if (ak==null || !nd.isInstanceOf[Element] || ak==vTag) Seq.empty
      else if (ak._1) {
        nd.asInstanceOf[Element].getAttributeNodeNS(ak._2,ak._4) match {
          case null => Seq.empty
          case a    => Seq(a)
        }
      } else {
        nd.asInstanceOf[Element].getElementsByTagNameNS(ak._2,ak._4) match {
          case null => Seq.empty
          case a    => NodeListIterator(a).toSeq
        }
      }
    }

    //finds the first "user" children of the Node having the given key ; includes both attributes and elements if appropriate
    def findNode(nd:Node, key:String, idx:Int): Node = {
      val ak = analyzeKey(key)
      if (ak==null || !nd.isInstanceOf[Element] || ak==vTag)
        null
      else if (ak._1)
        if (idx==0) nd.asInstanceOf[Element].getAttributeNodeNS(ak._2,ak._4) else null
      else {
        val r = nd.asInstanceOf[Element].getElementsByTagNameNS(ak._2,ak._4)
        if (r.getLength>0) r.item(idx) else null
      }
    }

    //builds a new node from V and a set of (K,T), using the appropriate data from this Param
    def buildNode(v: Option[V], t: GenTraversableOnce[(String, T)]):Node = {
      val elt = mkTopNode(v)
      for (x <- t) if (!stripEmpty || !canIgnore(x._2.elt))
        adoptNode(elt,x._2.elt,x._1)
      elt
    }

    //finds the text value for V in the Node
    def getTextValue(nd:Node):String = nd match {
      case a:Attr    => a.getValue
      case e:Element => if (vTag!=null && vTag._1) e.getAttributeNS(vTag._2,vTag._3)
                        else                       e.getFirstChild.getTextContent
    }

    //tells whether nd has 'proper' children (i.e. these children that contain values)
    def hasChildren(nd:Node):Boolean = nd match {
      case e:Element => e.getChildNodes.getLength match {
        case 0 => false
        case 1 => e.getLastChild match {
          case t:Text    => false
          case e:Element => vTag==null || !(e.getLocalName==vTag._3 && e.getNamespaceURI==vTag._2)
        }
        case _ => true
      }
      case _ => false
    }

    //tells whether nd is meaningless (has no value, no children)
    def canIgnore(nd:Node):Boolean = !hasChildren(nd) && (getData(nd) match {
      case null | None => true
      case _           => false
    })

    //update key in parent with given Node
    def update(parent:Node, key:String, nd:Node, idx:Int):Unit = parent match {
      case e:Element => findNode(parent,key,idx) match {
        case null => adoptNode(e, nd, key)
        case e    => parent.replaceChild(e, nd)
      }
      case _ => throw new IllegalStateException(s"a attribute node cannot hold any children: key=$key")
    }

  }

}