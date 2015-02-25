package utils.tree

import scala.collection.Map
import scala.collection.GenTraversableOnce
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import org.w3c.dom.{Document,Element,Node,NodeList}
import javax.xml.transform.{TransformerFactory,OutputKeys}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import java.io.Writer

/** This provides an implementation based on DOM.
 *  This is costly to use, but provides access to advanced features from the W3 world, such as xpath or xquery.
 *  - keys are element names
 *  - the PrefixTree node is stored in the user data (key "")
 *  - the DOM node may have a text representation for V in its last child OR as an attribute
 *  - the DOM may flatten terminal leaves into attributes
 *  This DOM contains only Element , Attribute and Text nodes. In any case, it contains:
 *  - one Element or Attribute for each child entry
 *  - possibly one Text or Attribute child for the stored V string value
 *  The DOM provides the whole navigation/storage infrastructure and the DOMPrefixTree class provides only the
 *  glue to PrefixTreeLike: DOMPrefixTree is mostly a wrapper
 */
abstract class DOMPrefixTree[+V] protected extends PrefixTreeLike.Abstract[String,V,DOMPrefixTree[V]] {
  import DOMPrefixTree._
  def elt:Element
  def value:Option[V]
  def update1[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:(String,T))(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator++Some(kv),default)
  def -(key: String): Repr = newBuilder(value,iterator.filter(_._1==key),default)
  def get(key: String): Option[Repr] = Option(elt.getElementsByTagName(key).item(0))
  /** additional method to recover all children having the same tag */
  def getAll(key: String): Iterator[Repr] = elt.getElementsByTagName(key).map(DOMPrefixTree.getNode[V,Repr])
  /* overridden for efficiency or necessity */
  override def isEmpty: Boolean = elt.getChildNodes.getLength<=1 && size==0
  override def foreach[U](f: ((String,Repr)) => U): Unit = iterator.foreach(f)
  override def update[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:GenTraversableOnce[(String,T)])(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator ++ kv,default)

  /** XML representation of this Node
   */
  def asXml(out:Writer, indent:Boolean) = {
     val tf = TransformerFactory.newInstance.newTransformer
     tf.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
     tf.setOutputProperty(OutputKeys.INDENT, if (indent) "yes" else "no")
     tf.transform(new DOMSource(elt), new StreamResult(out))
     out.flush
  }
}

/** The PrefixTree object contains:
 *  - concrete implementations for PrefixTree
 *  - a factory for building the most appropriate implementation
 */
object DOMPrefixTree extends PrefixTreeLikeBuilder.Factory1i[String] {
  type Tree[v] = DOMPrefixTree[v]
  type P0[v] = Params[v,DOMPrefixTree[v]]

  implicit def getNode[V,R<:DOMPrefixTree[V]](elt:Node):R = elt.getUserData("").asInstanceOf[R]

  implicit class NodeListIterator(val nd:NodeList) extends Iterator[Node] {
    val lg = nd.getLength-1
    var i  = -1
    override def length = lg
    def hasNext: Boolean = i<lg
    def next: Node = { i+=1; nd.item(i) }
  }

  /** The actual Parameters required to build a DOMPrefixTree.
   *  It contains many settings.
   *  - noDefault is from the parent
   *  - stripEmpty is from the parent
   *  - doc is the document used for the DOM ; it should change for each tree, and this prevents any implicit default //XXX check this
   *  - topTag is the tag to be used for the top node
   *  - toText is a method to convert V to a text value ; if null the value is not output as text
   *  - valueTag is the name of the tag for the value ; if null or "", the value is output as a text node that comes last
   *    otherwise is output either as an Element containing a single TextNode or an attribute, depending on how the tag gets analyzed
   *  - asAttr is a method that determines when a tag should be output as an attribute:
   *    1) whatever the return value, an attribute is only valid for a terminal leaf
   *    2) if asAttr returns either null or "" for the tag, then the result is an Element, otherwise an attribute
   */
  class Params[V,T<:Tree[V] with PrefixTreeLike[String,V,T]](noDefault:String=>T,stripEmpty:Boolean,val doc:Document,val topTag:String,val toText:V=>String,val valueTag:String,val asAttr:String=>String)
        extends super.Params[V,T](noDefault,stripEmpty) {
    val valueAttrTag = if (asAttr==null) null else if (valueTag!=null && valueTag.length>0) asAttr(valueTag) else null
  }
  object Params {
    //a simplified factory with standard defaults ; attributes are recognized by starting with @ (which anyway is not valid for elements)
    def apply[V](doc:Document,toText:V=>String,attrTag:String):Params[V,Tree[V]] = new Params[V,Tree[V]](PrefixTreeLikeBuilder.noElt,true,doc,"_",toText,attrTag,x=>{if (x(0)=='@') x.substring(1) else null })
  }

  /** The first concrete PrefixTree class.
   *  It is designed to be sub-classed to minimize memory footprint.
   *  It is public in case one wants to derive other compatible implementations.
   *  It's constructor is protected because we want users to only rely on PrefixTree[K,V] and use the factory method.
   *  params is made implicit to facilitate code writing below.
   */
  class Abstract[V](val elt:Element, val value:Option[V])(implicit val params:P0[V]) extends DOMPrefixTree[V] with super.Abstract[V] {
    elt.setUserData("", this, null)  //XXX data handler
    if (value!=None && params.toText!=null) {
      val txt = params.toText(value.get)
      //case 1: V text is an attribute
      if (params.valueAttrTag!=null && params.valueAttrTag.length>0)
        elt.setAttribute(params.valueAttrTag,txt)
      //case 2: V text is a text node inside a standard element node
      else if (params.valueTag!=null && params.valueTag.length>0) {
        val elt1 = params.doc.createElement(params.valueTag)
        elt1.appendChild(params.doc.createTextNode(txt))
        elt.appendChild(elt1)
      //case 3: V text is a text node
      } else {
        elt.appendChild(params.doc.createTextNode(txt))
      }
    }
    def iterator:Iterator[(String, Repr)] = elt.getChildNodes.filter { x=>
      //yields all children: must remove V text node/attribute (and possible super element) but keep possible leaves flattened as attributes
      x.getNodeType match {
        case Node.ELEMENT_NODE   => params.valueTag!=x.getNodeName     //sub child if doesn't contain the V text representation
        case Node.ATTRIBUTE_NODE => params.valueAttrTag!=x.getNodeName //sub child if doesn't contain the V text representation
        case Node.TEXT_NODE      => false                              //never a proper sub child
      }
    }.map(e=>(e.getNodeName,e))
    override def newBuilder = super[Abstract].newBuilder
  }

  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass Abstract so as to minimize the memory footprint for each node.
   */
  implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[String, V, DOMPrefixTree[V]] { type Params=P0[V] } = {
    new PrefixTreeLikeBuilder[String, V, DOMPrefixTree[V]] {
      type Params = P0[V]
      val params:Params = p
      def newEmpty:PrefixTreeLikeBuilder[String,V,Repr] = builder[V](params)

      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = {
        val elt = params.doc.createElement(params.topTag)
        for (x <- t) if (!p.stripEmpty || !x._2.isNonSignificant) {
          val attr = if (params.asAttr!=null) params.asAttr(x._1) else null
          if (attr==null || attr.length==0 || x._2.size>0) //standard node
            elt.appendChild(params.doc.renameNode(x._2.elt, null, x._1))
          else { //degen node as attribute
            val a = params.doc.createAttribute(attr)
            a.setValue("")
            elt.appendChild(a)
          }
        }
        new Abstract[V](elt,v)
      }
    }
  }
}