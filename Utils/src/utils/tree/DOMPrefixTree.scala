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
 *  - the DOM node may have a text representation for V in its first child OR as an attribute
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
 *      e.g. <tag>vtxtvalue<child1>...</child1>...<childn>...</childn></tag>
 *    - it can appear as an element containing a single text node
 *      e.g. <tag><val>vtxtvalue<val><child1>...</child1>...<childn>...</childn></tag>
 *  o leaves (no children) may appear either as element or as attributes
 *      e.g. <tag val="vtxtvalue" terminal1="t1vtxtvalue"><child2>...</child2>...<childn>...</childn></tag>
 *
 *  Note that xsl transformations won't build a DOMPrefixTree, and it's useless to expect to succeed in that.
 */
abstract class DOMPrefixTree[+V] protected extends PrefixTreeLike.Abstract[String,V,DOMPrefixTree[V]] with DOMHelper.DOMContainer { self=>
  import DOMPrefixTree._
  type Params <: DOMHelper.Params[v,t] with PrefixTreeLike.Params[String,v,t] forSome { type v<:V; type t<:Tree[v] }
  //we have a specific builder with additional methods
  protected[this] override def newBuilder:DOMPrefixTree.Builder[V,Repr] { type Params = self.Params } = ???

  def update1[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:(String,T))(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator++Seq(kv),default)
  def -(key: String): Repr = newBuilder(value,iterator.filter(_._1==key),default)
  /* overridden for efficiency or necessity */
  override def foreach[U](f: ((String,Repr)) => U): Unit = iterator.foreach(f)
  override def update[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:GenTraversableOnce[(String,T)])(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator ++ kv,default)

  //the implemention of most methods is diverted to Params, i.e. DOMHelper!
  def value:Option[V]                   = DOMHelper.getData(elt)
  def iterator:Iterator[(String, Repr)] = params.iterator(elt)
  def getTextValue:String               = params.getTextValue(elt)
  override def isEmpty: Boolean         = !params.hasChildren(elt)
  override def isNonSignificant:Boolean = params.canIgnore(elt)
  def get(key: String): Option[Repr]    = get(key,0)

  //additionnal methods
  /** recovers all children having the same tag */
  def getAll(key: String): Seq[Repr]
  def get(key:String,idx:Int):Option[Repr]
  def apply(key:String,idx:Int):Repr

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
  type P0[v]   = Params[v]
  type Bld[v]  = Builder[v,Tree[v]]

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
  class Params[V](noDefault:String=>Tree[V],stripEmpty:Boolean,val doc:Document,val topTag:String, val toText:V=>String, val valueTag:String, val toXMLname:String=>String, val defaultNamespace:String, val namespaces:(String,String)*)
        extends super.Params[V,Tree[V]](noDefault,stripEmpty) with DOMHelper.Params[V,Tree[V]] {
    def toT(nd:Node) = fact(nd)
  }

  object Params {
    //a simplified factory with standard defaults ; attributes are recognized by starting with @ (which anyway is not valid for elements)
    def apply[V](doc:Document,toText:V=>String,attrTag:String) =
      new Params[V](PrefixTreeLikeBuilder.noElt,true,doc,"_",toText,attrTag,null,null)
    //a full factory
    def apply[V](noDefault:String=>Nothing,stripEmpty:Boolean,doc:Document,topTag:String,toText:V=>String,valueTag:String,toXmlTag:String=>String,defaultNS:String,namespaces:(String,String)*) =
      new Params[V](noDefault,stripEmpty,doc,topTag,toText,valueTag,toXmlTag,defaultNS,namespaces:_*)
    //an almost full factory with common settings for noDefault and stripEmpty
    def apply[V](doc:Document,topTag:String,toText:V=>String,valueTag:String,toXmlTag:String=>String,defaultNS:String,namespaces:(String,String)*) =
      new Params[V](PrefixTreeLikeBuilder.noElt,true,doc,topTag,toText,valueTag,toXmlTag,defaultNS,namespaces:_*)
  }

  /** The concrete wrapper.
   *  All it takes is a Node that contains the appropriate (Option[V],P0[V]) in its user data key "".
   *  Note that of course, children must have been themselves properly built!
   */
  final class Abstract[V](val elt:Node) extends DOMPrefixTree[V] with super.Abstract[V] {
    val params:P0[V] = DOMHelper.getParams(elt)
    def getAll(key: String): Seq[Repr] = params.findAll(elt, key).map(new Abstract(_))
    def get(key: String, idx:Int): Option[Repr] = Option(params.findNode(elt, key, idx)).map(new Abstract(_))
    def apply(key: String, idx:Int): Repr = params.findNode(elt, key, idx) match {
      case null => noKey(key)
      case nd   => new Abstract(nd)
    }
    override def newBuilder = super[Abstract].newBuilder
  }

  /** The PrefixTreeLikeBuilder for DOM has an additional constructor (from DOM Node)
   */
  abstract class Builder[V,Tree<:PrefixTreeLike[String,V,Tree]] extends PrefixTreeLikeBuilder[String, V, Tree] {
    def apply(elt:Node):Repr
  }

  /** The factory mostly deals with building the appropriate DOM tree representation!
   *  The DOMPrefixTree result is only a simple result.
   */
  implicit def builder[V](implicit p:P0[V]) =
    new Builder[V, Tree[V]] {
      type Params = P0[V]
      val params:Params = p
      def newEmpty:Bld[V] { type Params=P0[V] } = builder[V](params)
      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = apply(params.buildNode(v, t))
      def apply(elt:Node):Repr = new Abstract[V](elt)
    }

  def apply[V](elt:Node):Tree[V] = new Abstract(elt)
}