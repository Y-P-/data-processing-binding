package utils.tree

import scala.collection.GenTraversableOnce
import org.w3c.dom.{Document,Element,Attr,Node,NodeList,NamedNodeMap,UserDataHandler}
import org.w3c.dom.Node._
import javax.xml.XMLConstants

/** Contains some utilities for building DOM trees in a specific way.
 *  These are of course used by DOMPrefixTree, but they don't depend in any way on the PrefixTree class.
 */
object DOMHelper {

  def getData[V](elt:Node):Option[V] =
    elt.getUserData("").asInstanceOf[Option[V]]
  def setData[V](elt:Node,v:Option[V]):Unit =
    elt.setUserData("",v,handler)

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

  /** Handler for our data: copy!
   */
  object handler extends UserDataHandler {
    import UserDataHandler._
    def handle(op:Short,key:String,data:Object,src:Node,dst:Node) = {
      if (op==NODE_CLONED)
        dst.setUserData(key, data, handler)
      else if (op!=NODE_DELETED)
        src.setUserData(key, data, handler)
    }
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

    def isTreeNode(nd:Node) = nd.getNodeType match {
      case TEXT_NODE      => false                                       //no Text node match a tree Node
      case ATTRIBUTE_NODE => if (nd.getNodeName.startsWith("xml")) false  //special attribute cannot match (restricted name)
                     else if (vTag._1) !(nd.getLocalName==vTag._3 && nd.getNamespaceURI==vTag._2) //must not match text attribute
                     else true
      case ELEMENT_NODE   => vTag==null || vTag._1 || !(nd.getLocalName==vTag._3 && nd.getNamespaceURI==vTag._2) //must not match text node
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

    //creates/updates the appropriate text representation
    def mkTxtNode(node:Node,v:Option[V]):Unit = if (v!=null && v!=None && toText!=null) {
      val txt = toText(v.get)
      if (node.getNodeType==ELEMENT_NODE) {
        val elt = node.asInstanceOf[Element]
        if (vTag==null) {                        //case 1: V text is a text node
          val t = doc.createTextNode(txt)
          if (elt.getFirstChild==null)           //initial creation
            elt.appendChild(t)                   //append (in first position actually)
          else if (elt.getFirstChild.getNodeType==TEXT_NODE)
            elt.replaceChild(t, elt.getFirstChild) //otherwise replace
          else                                   //there was yet no value txt
            elt.insertBefore(t, elt.getFirstChild)
        } else if (vTag._1) {                    //case 2: V text is an attribute
          val nd = doc.createAttributeNS(vTag._2,vTag._3)
          nd.setValue(txt)
          elt.setAttributeNode(nd)
        } else {                                 //case 3: V text is a text node inside a standard element node
          val t = doc.createTextNode(txt)
          if (elt.getFirstChild==null) {
            val nd = doc.createElementNS(vTag._2,vTag._3) //build container element
            nd.appendChild(t)
            elt.appendChild(nd)
          } else {
            val nd1 = elt.getElementsByTagNameNS(vTag._2,vTag._3)
            if (nd1.getLength==0) {               //no text node yet : insert it in the first position
              val nd = doc.createElementNS(vTag._2,vTag._3) //build container element
              nd.appendChild(t)
              elt.insertBefore(nd, elt.getFirstChild)
            } else {                              //update current text node
              nd1.item(0).replaceChild(t, nd1.item(0).getFirstChild)
            }
          }
        }
      } else if (node.getNodeType==ATTRIBUTE_NODE) {
        val att = node.asInstanceOf[Attr]
        att.setValue(txt)
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
      mkTxtNode(nd,v)
      nd
    }

    //renames node with a new key
    def renameNode(elt:Node, key:String):Node = {
      val ak = analyzeKey(key)
      if (ak!=null) {
        val nd0 = if (ak._1) {
          if (hasChildren(elt))
            throw new IllegalStateException(s"a non empty Node cannot be transformed as an attribute: key=$key")
          val nd = doc.createAttributeNS(ak._2,ak._3)
          val txt = getTextValue(elt)
          if (txt!=null)
            nd.setValue(txt)
          nd
        } else {
          doc.renameNode(elt, ak._2, ak._3)
        }
        setData(nd0, getData[V](elt))
        if (key!=ak._3)
          nd0.setUserData("$", key, handler)
        nd0
      } else
        null
    }

    //parent appends node with new key ; if an attribute, the previous attribute with the same name is replaced
    def appendNode(parent:Element, elt:Node):Unit = if (elt!=null) elt.getNodeType match {
      case ELEMENT_NODE   => parent.appendChild(elt)
      case ATTRIBUTE_NODE => parent.setAttributeNodeNS(elt.asInstanceOf[Attr])
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

    //finds the child of the Node having the given key ; includes both attributes and elements if appropriate
    //returns null if not found
    def findNode(nd:Node, key:String, idx:Int): Node = findNodes(nd,key) match {
      case null => null
      case list => if (list.getLength>idx) list.item(idx) else null
    }

    //finds all nodes for a given key ; returns null if either the node is not eligible for children
    //or if the key is not eligible as a child key
    def findNodes(nd:Node, key:String): NodeList = {
      val ak = analyzeKey(key)
      if (ak==null || !nd.isInstanceOf[Element] || ak==vTag)
        null
      else if (ak._1) {
        new NodeList {
          def getLength = 1
          def item(idx:Int) = if (idx==0) nd.asInstanceOf[Element].getAttributeNodeNS(ak._2,ak._4) else throw new ArrayIndexOutOfBoundsException(idx)
        }
      } else {
        nd.asInstanceOf[Element].getElementsByTagNameNS(ak._2,ak._4)
      }
    }

    //builds a new node from V and a set of (K,T), using the appropriate data from this Param
    def buildNode(v: Option[V], t: GenTraversableOnce[(String, T)]):Node = {
      val elt = mkTopNode(v)
      for (x <- t) if (!stripEmpty || !canIgnore(x._2.elt))
        appendNode(elt,renameNode(x._2.elt,x._1))
      elt
    }

    //finds the text value for V in the Node
    def getTextValue(nd:Node):String = nd.getNodeType match {
      case ELEMENT_NODE   => val e = nd.asInstanceOf[Element]
                             if (vTag!=null && vTag._1) e.getAttributeNS(vTag._2,vTag._3)
                             else                       e.getFirstChild.getTextContent
      case ATTRIBUTE_NODE => nd.asInstanceOf[Attr].getValue
    }

    //tells whether nd has 'proper' children (i.e. these children that contain values)
    def hasChildren(nd:Node):Boolean = nd match {
      case e:Element => e.getChildNodes.getLength match {
        case 0 => false
        case 1 => e.getLastChild.getNodeType match {
          case TEXT_NODE    => false
          case ELEMENT_NODE => vTag==null || !(e.getLocalName==vTag._3 && e.getNamespaceURI==vTag._2)
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

    //update key/index in parent with given Node ; we do the best to preserve the node position
    //the Node may be converted if it is not the appropriate type for the key
    //if the key is not found and idx is 0, then append an appropriate child
    //if the parent is not eligible for children the method also fails
    //if nd is null, removes the corresponding child instead
    def update(parent:Node, key:String, idx:Int, nd:Node):Unit = parent.getNodeType match {
      case ELEMENT_NODE => {
        val p         = parent.asInstanceOf[Element]
        val nd0       = if (nd!=null) renameNode(nd, key) else null  //prepare new node with correct name; this may change nd type
        val toReplace = findNode(parent,key,idx)
        if (toReplace==null) {                        //nothing to update! check why and possibly append
          if (nd!=null) {                             //otherwise nothing to remove!
            if (idx==0) appendNode(p, nd0)              //only if idx==0
            else throw new IllegalArgumentException(s"updating $parent ($key,$idx) is not a valid operation")
          }
        } else if (nd!=null) { //note that nd0 and toReplace have the same kind by construction! (key defines type)
          toReplace.getNodeType match {
            case ELEMENT_NODE   => p.replaceChild(nd0, toReplace)
            case ATTRIBUTE_NODE => p.setAttributeNode(nd0.asInstanceOf[Attr])
          }
        } else {
          toReplace.getNodeType match {
            case ELEMENT_NODE   => p.removeChild(toReplace)
            case ATTRIBUTE_NODE => p.removeAttributeNode(toReplace.asInstanceOf[Attr])
          }
        }
      }
      case _ => throw new IllegalArgumentException(s"updating an non Element node with $key is not a valid operation")
    }

    //replace key in parent with all given Nodes ; inserted nodes may have their type changed on update.
    def update(parent:Node, key:String, nodes:GenTraversableOnce[Node]):Unit = parent.getNodeType match {
      case ELEMENT_NODE => {
        val p         = parent.asInstanceOf[Element]
        val toReplace = findNodes(parent,key)
        if (toReplace==null)
          throw new IllegalArgumentException(s"updating $parent ($key) is not a valid operation")
        else if (toReplace.getLength==0) {   //nothing to update! check why and possibly append
          for (nd <- nodes) appendNode(p, renameNode(nd, key))
        } else {
          var idx = 0
          if (toReplace.item(0).getNodeType==ATTRIBUTE_NODE) {
            for (nd <- nodes) {
              if (idx>1) throw new IllegalArgumentException(s"updating $parent ($key) with multiple attributes is not a valid operation")
              p.setAttributeNode(renameNode(nd, key).asInstanceOf[Attr])
              idx += 1
            }
          } else {
            for (nd <- nodes) {
              val e1  = toReplace.item(idx)
              val nd1 = renameNode(nd, key)
              if (e1!=null) p.replaceChild(nd1, e1) else p.appendChild(nd1)
              idx += 1
            }
          }
        }
      }
      case _ => throw new IllegalArgumentException(s"updating an non Element node with $key is not a valid operation")
    }

  }

}