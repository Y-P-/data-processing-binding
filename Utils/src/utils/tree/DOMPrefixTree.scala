package utils.tree

import scala.collection.Map
import scala.collection.GenTraversableOnce
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import org.w3c.dom.{Document,Element,Node,NodeList,NamedNodeMap,Attr}
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
 *  - order is respected as possible, but any item that maps as an attribute will likely see its place changed
 *  In this implementation the DOM provides the whole navigation/storage infrastructure and the DOMPrefixTree
 *  class provides only the glue to PrefixTreeLike: DOMPrefixTree is only a wrapper, which is built on demand.
 *  Because of this property, any subtree extracted for the original can easily be traversed as a PrefixTree:
 *  no specific rebuilding is necessary.
 *  Multiple DOM representation are possible:
 *  o the text value for V may or may not appear in the DOM. If it does:
 *    - it can appear as an attribute
 *    - it can appear as a text node
 *    - it can appear as an element containing a single text node
 *  o leaves (no children) may appear either as element, which may be empty depending on the previous choice
 *    or as attributes (in which case the V text data will be the value if output)
 */
abstract class DOMPrefixTree[+V] protected extends PrefixTreeLike.Abstract[String,V,DOMPrefixTree[V]] {
  import DOMPrefixTree._
  def elt:Node
  def value:Option[V]
  def update1[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:(String,T))(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,iterator++Some(kv),default)
  def -(key: String): Repr = newBuilder(value,iterator.filter(_._1==key),default)
  /** additional method to recover all children having the same tag */
  def getAll(key: String): Iterator[Repr]
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

  protected def getData[V](elt:Node):Option[V] = elt.getUserData("").asInstanceOf[Option[V]]
  protected def getParams[V](elt:Node):P0[V]   = elt.getOwnerDocument.getUserData("").asInstanceOf[P0[V]]


  implicit class NodeListIterator(val nd:NodeList) extends Iterator[Node] {
    val lg = nd.getLength-1
    var i  = -1
    override def length = lg
    def hasNext: Boolean = i<lg
    def next: Node = { i+=1; nd.item(i) }
  }
  implicit class AttrListIterator(val nd:NamedNodeMap) extends Iterator[Node] {
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
   *  - doc is the document used for the DOM ; it should change for each tree, and this prevents any implicit default
   *    it is modified to contain this Params value as user data so as to share it between all children instances
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
    doc.setUserData("", this, null)
    val valueAttrTag = if (asAttr==null) null else if (valueTag!=null && valueTag.length>0) asAttr(valueTag) else null
    //returns the attribute key for a input key name, or null if not appropriate
    def attr(s:String) = if (asAttr==null) null else { val r=asAttr(s); if (r==null || r.length==0) null else r }
    //returns the text value for v or null if not appropriate
    def text(v:Option[V]) = if (toText==null || v==None) null else toText(v.get)
  }
  object Params {
    //a simplified factory with standard defaults ; attributes are recognized by starting with @ (which anyway is not valid for elements)
    def apply[V](doc:Document,toText:V=>String,attrTag:String):Params[V,Tree[V]] = new Params[V,Tree[V]](PrefixTreeLikeBuilder.noElt,true,doc,"_",toText,attrTag,x=>{if (x(0)=='@') x.substring(1) else null })
    def apply[V](noDefault:String=>Nothing,stripEmpty:Boolean,doc:Document,topTag:String,toText:V=>String,valueTag:String,asAttr:String=>String):Params[V,Tree[V]] = new Params[V,Tree[V]](noDefault,stripEmpty,doc,topTag,toText,valueTag,asAttr)
  }

  /** The concrete wrapper.
   *  All it takes is a Node that contains the appropriate (Option[V],P0[V]) in its user data key "".
   *  Note that of course, children must have been themselves properly built!
   */
  implicit final class Abstract[V](val elt:Node) extends DOMPrefixTree[V] with super.Abstract[V] {
    def value:Option[V] = getData(elt)
    val params:P0[V]    = getParams(elt)
    def iterator:Iterator[(String, Repr)] = {
      val i0 = elt.getChildNodes.filter { x=>
        //yields all children: only keep Element which don't match the possible V text node
        x.getNodeType==Node.ELEMENT_NODE && params.valueTag!=x.getNodeName
      }
      val i = if (params.asAttr!=null) { //some legitimate nodes may appear as attributes
        val i1 = elt.getAttributes.filter { x=>
          params.valueAttrTag!=x.getNodeName  //the V text value attribute would not qualify
        }
        i1 ++ i0
      } else i0
      i.map { e=>
        val name = e match {
          case a:Attr    => a.getUserData("$").asInstanceOf[String]
          case n:Element => n.getNodeName
        }
        (name,new Abstract[V](e))
      }
    }
    def get(key: String): Option[Repr] = elt match {
      case e:Element => e.getElementsByTagName(key).item(0) match {
                          case null => params.attr(key) match {
                            case null => None
                            case attr => Some(new Abstract[V](e.getAttributeNode(attr)))
                          }
                          case e1 => Some(new Abstract[V](e1))
                        }
      case _ => None
    }
    def getAll(key: String): Iterator[Repr] = elt match {
      case e:Element => val l=e.getElementsByTagName(key)
                        if (l.getLength>0) l.map { e1=> new Abstract[V](e1) }
                        else {
                          params.attr(key) match {
                            case null => Iterator.empty
                            case attr => Some(new Abstract[V](e.getAttributeNode(attr))).iterator
                          }
                        }
      case _         => Iterator.empty
    }
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

      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = {
        val elt = params.doc.createElement(params.topTag)
        elt.setUserData("", v, null)  //XXX data handler
        for (x <- t) if (!params.stripEmpty || !x._2.isNonSignificant) {
          val attr = params.attr(x._1)
          if (attr==null || !x._2.isEmpty) //standard node
            elt.appendChild(params.doc.renameNode(x._2.elt, null, x._1))
          else { //degen existing node as attribute
            val a = params.doc.createAttribute(attr)
            val v1 = getData[V](x._2.elt)
            a.setUserData("", v1, null)
            a.setUserData("$", x._1, null)  //keep initial name
            val t1 = params.text(v1)
            if (t1!=null) a.setValue(t1) //recompute text rather than find it
            elt.setAttributeNode(a)
          }
        }
        val txt = params.text(v)
        if (txt!=null) {
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
        new Abstract[V](elt)
      }
    }
  }
}