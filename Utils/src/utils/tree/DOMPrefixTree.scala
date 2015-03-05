package utils.tree

import scala.collection.Map
import scala.collection.GenTraversableOnce
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import org.w3c.dom.{Document,Element,Node,NodeList,NamedNodeMap,Attr,Text}
import javax.xml.transform.{TransformerFactory,OutputKeys}
import javax.xml.transform.dom.{DOMSource,DOMResult}
import javax.xml.transform.stream.{StreamResult,StreamSource}
import javax.xml.XMLConstants
import javax.xml.xpath.XPath
import javax.xml.xpath.XPathConstants
import javax.xml.xpath.XPathExpression
import javax.xml.xpath.XPathExpressionException
import javax.xml.xpath.XPathFactory
import java.io.Writer

/** This provides an implementation based on DOM.
 *  This is costly to use, but provides access to advanced features from the W3 world, such as xpath or xquery.
 *  - keys are (almost) element names
 *  - the PrefixTree node is stored in the user data (key "")
 *  - the DOM node may have a text representation for V in its last child OR as an attribute
 *  - the DOM may flatten terminal leaves into attributes
 *  This DOM contains only Element, Attribute and Text nodes. In any case, it contains:
 *  - one Element or Attribute for each child entry
 *  - possibly one Text or Attribute child for the stored V string value
 *  - order is respected as possible, but any item that maps as an attribute may see its place changed
 *
 *  In this implementation the DOM provides the whole navigation/storage infrastructure and the DOMPrefixTree
 *  class provides only the glue to PrefixTreeLike: DOMPrefixTree is only a wrapper, which is built on demand.
 *  Because of this property, any subtree extracted for the original can easily be traversed as a PrefixTree:
 *  no specific rebuilding is necessary. However, this prevents using default.
 *
 *  Names have a specific problem, because XML doesn't accept any input.
 *  Furthermore, we may want attributes and some way to distinguish them.
 *  A translation method is given, to convert from any String into an acceptable xml tag. This tag is prefixed
 *  by @ if we want it to appear as an attribute.
 *  However, while the DOM only knows the XML names, the associated DOMPrefixTree only knows about the original
 *  names, not the XML name.
 *  It is possible to use namespaces provided the prefix are declared in the Params used for building the tree.
 *
 *  Multiple DOM representation are possible:
 *  o the text value for V may or may not appear in the DOM. If it does:
 *    - it can appear as an attribute
 *      e.g. <tag val="vtxtvalue"><child1>...</child1>...<childn>...</childn></tag>
 *    - it can appear as a text node
 *      e.g. <tag><child1>...</child1>...<childn>...</childn>vtxtvalue</tag>
 *    - it can appear as an element containing a single text node
 *      e.g. <tag><child1>...</child1>...<childn>...</childn><val>vtxtvalue<val></tag>
 *  o leaves (no children) may appear either as element or as attributes
 *      e.g. <tag val="vtxtvalue" terminal1="t1vtxtvalue"><child2>...</child2>...<childn>...</childn></tag>
 *
 *  Note that xsl transformations won't build a DOMPrefixTree, and it's useless to expect to succeed in that.
 */
abstract class DOMPrefixTree[+V] protected extends PrefixTreeLike.Abstract[String,V,DOMPrefixTree[V]] {
  import DOMPrefixTree._
  type Params <: DOMPrefixTree.P0[_<:V]
  def elt:Node
  def value:Option[V]
  def update1[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:(String,T))(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator++Seq(kv),default)
  def -(key: String): Repr = newBuilder(value,iterator.filter(_._1==key),default)
  /** recovers all children having the same tag */
  def getAll(key: String): Seq[Repr]
  /** text value for V */
  def getTextValue:String
  /* overridden for efficiency or necessity */
  override def foreach[U](f: ((String,Repr)) => U): Unit = iterator.foreach(f)
  override def update[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:GenTraversableOnce[(String,T)])(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator ++ kv,default)

  /** XML representation of this Node
   */
  def asXml(out:Writer, indent:Boolean):Unit = {
     val tf = TransformerFactory.newInstance.newTransformer
     tf.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
     tf.setOutputProperty(OutputKeys.INDENT, if (indent) "yes" else "no")
     tf.transform(new DOMSource(elt), new StreamResult(out))
     out.flush
  }

  def transform(in:StreamSource,doc:Document):Node = {
    val factory = TransformerFactory.newInstance
    val template = factory.newTemplates(in)
    val transformer = factory.newTransformer
    val result = new DOMResult(doc)
    transformer.transform(in, result)
    result.getNode
  }
  def transform(in:String,doc:Document):Node = transform(new StreamSource(in),doc)

  def find(expr:String):NodeList = {
    val xpathFactory = XPathFactory.newInstance
    val xpath = xpathFactory.newXPath
    val x = xpath.compile(expr)
    x.evaluate(elt,XPathConstants.NODESET).asInstanceOf[NodeList]
  }

}

/** The PrefixTree object contains:
 *  - concrete implementations for PrefixTree
 *  - a factory for building the most appropriate implementation
 */
object DOMPrefixTree extends PrefixTreeLikeBuilder.Factory1i[String] {
  type Tree[v] = DOMPrefixTree[v]
  type P0[v] = Params[v,DOMPrefixTree[v]]

  protected def getData[V](elt:Node):Option[V] = elt.getUserData("").asInstanceOf[Option[V]]
  protected def getParams[V](elt:Node):P0[V]   = {
    val p = elt.getOwnerDocument.getUserData("")
    if (p==null) throw new IllegalArgumentException("The owner document is not a valid document for a DOMPrefixTree")
    p.asInstanceOf[P0[V]]
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

  /** The actual Parameters required to build a DOMPrefixTree.
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
  class Params[V,T<:Tree[V] with PrefixTreeLike[String,V,T]](noDefault:String=>T,stripEmpty:Boolean,val doc:Document,topTag:String,toText:V=>String,valueTag:String,toXMLname:String=>String,defaultNamespace:String,namespaces:(String,String)*)
        extends super.Params[V,T](noDefault,stripEmpty) {
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
      nd.setUserData("", v, null)
      if (defaultNamespace!=null)
        nd.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns", defaultNamespace)
      if (hasNS) for (x <- namespaces)
        nd.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns:" + x._2, x._1)
      if (v!=null && v!=None && toText!=null)
        mkTxtNode(nd,toText(v.get))
      nd
    }

    //top adopt node with new key
    def adoptNode(top:Element, pNode:DOMPrefixTree[V], key:String):Unit = {
      val ak = analyzeKey(key)
      if (ak!=null) {
        val elt = pNode.elt
        val nd0 = if (ak._1) {
          if (!pNode.isEmpty)
            throw new IllegalStateException(s"a non empty Node cannot be transformed as an attribute: key=$key, value=${pNode.value}")
          val nd = doc.createAttributeNS(ak._2,ak._3)
          nd.setUserData("", getData[V](elt), null)
          val txt = pNode.getTextValue
          if (txt!=null)
            nd.setValue(txt)
          top.setAttributeNodeNS(nd)
          nd
        } else {
          val nd = doc.renameNode(elt, ak._2, ak._3)
          top.appendChild(nd)
          nd
        }
        if (key!=ak._3)
          nd0.setUserData("$", key, null)
      }
    }

    //returns the triplet (isAttribute,namespace,local XML name) for a given string ; eliminates matches with the valueTag (conflict)
    def xmlName(s:String):(Boolean,String,String) = {
      val r= (if (toXMLname==null) s else toXMLname(s)) match {
        case `valueTag` => null
        case t          => t
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
    def iterator(nd:Node): Iterator[(K,DOMPrefixTree[V])] = {
      val it:Iterator[Node] = (AttrListIterator(nd.getAttributes)++NodeListIterator(nd.getChildNodes)).filter(filter)
      it.map { e=>
        val name = e.getUserData("$") match {
          case null     => e.getNodeName
          case s:String => s
        }
        (name,new Abstract[V](e))
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
    def findFirst(nd:Node, key:String): Node = {
      val ak = analyzeKey(key)
      if (ak==null || !nd.isInstanceOf[Element] || ak==vTag)
        null
      else if (ak._1)
        nd.asInstanceOf[Element].getAttributeNodeNS(ak._2,ak._4)
      else {
        val r = nd.asInstanceOf[Element].getElementsByTagNameNS(ak._2,ak._4)
        if (r.getLength>0) r.item(0) else null
      }
    }

    //builds a new node from V and a set of (K,T), using the appropriate data from this Param
    def buildNode(v: Option[V], t: GenTraversableOnce[(K, T)]):Node = {
      val elt = mkTopNode(v)
      for (x <- t) if (!stripEmpty || !x._2.isNonSignificant)
        adoptNode(elt,x._2,x._1)
      elt
    }

    //finds the text value for V in the Node
    def getTextValue(nd:Node):String = nd match {
      case a:Attr    => a.getValue
      case e:Element => if (vTag!=null && vTag._1) e.getAttributeNS(vTag._2,vTag._3)
                        else                       e.getLastChild.getTextContent
    }

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
  }

  object Params {
    //a simplified factory with standard defaults ; attributes are recognized by starting with @ (which anyway is not valid for elements)
    def apply[V](doc:Document,toText:V=>String,attrTag:String):Params[V,Tree[V]] =
      new Params[V,Tree[V]](PrefixTreeLikeBuilder.noElt,true,doc,"_",toText,attrTag,null,null)
    //a full factory
    def apply[V](noDefault:String=>Nothing,stripEmpty:Boolean,doc:Document,topTag:String,toText:V=>String,valueTag:String,toXmlTag:String=>String,defaultNS:String,namespaces:(String,String)*):Params[V,Tree[V]] =
      new Params[V,Tree[V]](noDefault,stripEmpty,doc,topTag,toText,valueTag,toXmlTag,defaultNS,namespaces:_*)
  }

  def setNamespace(elt:Element, ns:String, prefix:String) =
    elt.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns:" + prefix, ns)

  /** The concrete wrapper.
   *  All it takes is a Node that contains the appropriate (Option[V],P0[V]) in its user data key "".
   *  Note that of course, children must have been themselves properly built!
   */
  final class Abstract[V](val elt:Node) extends DOMPrefixTree[V] with super.Abstract[V] {
    def value:Option[V] = getData(elt)
    val params:P0[V]    = getParams(elt)
    def iterator:Iterator[(String, Repr)] = params.iterator(elt)
    def get(key: String): Option[Repr]    = Option(params.findFirst(elt, key)).map(new Abstract[V](_))
    def getAll(key: String): Seq[Repr]    = params.findAll(elt, key).map(new Abstract[V](_))
    def getTextValue:String               = params.getTextValue(elt)
    override def isEmpty: Boolean         = !params.hasChildren(elt)
    override def newBuilder = super[Abstract].newBuilder
  }

  /** The factory mostly deals with building the appropriate DOM tree representation!
   *  The DOMPrefixTree result is only a simple result.
   */
  implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[String, V, DOMPrefixTree[V]] { type Params=P0[V] } = {
    new PrefixTreeLikeBuilder[String, V, DOMPrefixTree[V]] {
      type Params = P0[V]
      val params:Params = p
      def newEmpty:PrefixTreeLikeBuilder[String,V,Repr] = builder[V](params)
      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = new Abstract[V](params.buildNode(v, t))
    }
  }

  def bind[V](elt:Node):DOMPrefixTree[V] = new Abstract(elt)
}