package utils.tree

import scala.collection.GenTraversableOnce
import org.w3c.dom.{Document,Node}

/** The Mutable version for DOMPrefixTree.
 *  It is invariant, but compatible with DOMPrefixTree (sub-class.)
 *  Because MutableDOMPrefixTree is also a wrapper, it is actually possible to easily mutate
 *  an "unmutable" DOMPrefixTree by extracting its Node, wrapping it up in a MutableDOMPrefixTree
 *  and applying modifications. Of course, this is highly unadvisable: use the mutable version
 *  if you're not certain that the tree will not mutate!
 */
abstract class MutableDOMPrefixTree[V] protected extends DOMPrefixTree[V] with PrefixTreeLike[String, V, MutableDOMPrefixTree[V]] with MutablePrefixTreeLike[String,V,MutableDOMPrefixTree[V]] {
  def value_=(v:Option[V]):Unit      = DOMHelper.setData(elt, v)
  def default_=(d:String=>Repr):Unit = throw new UnsupportedOperationException("DOMPrefixTrees don't support default")
  def update(k:String,t:MutableDOMPrefixTree[V]):Unit = params.update(elt, k, t.elt, 0)

  //additional methods, in particular in support of the multi-key capability of DOM
  def update(k:String,idx:Int,t:MutableDOMPrefixTree[V]):Unit = params.update(elt, k, t.elt, idx)
}

/** The factory for MutableDOMPrefixTree is almost cut/paste from the immutable version.
 *  But Params has to be different (underlying factory IS different.)
 *  So some code has to be written again...
 */
object MutableDOMPrefixTree extends PrefixTreeLikeBuilder.Factory1i[String] {
  type Tree[v] = MutableDOMPrefixTree[v]
  type P0[v]   = Params[v]
  type Bld[v]  = DOMPrefixTree.Builder[v,Tree[v]]  //the builder pattern from immutable is OK

  /** Copy from the immutable version!
   *  Of course different because the factory (this object) is different!
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

  /** Again copy from immutable version.
   */
  final class Abstract[V](val elt:Node) extends MutableDOMPrefixTree[V] with super.Abstract[V] {
    val params:P0[V] = DOMHelper.getParams(elt)
    def get(key: String): Option[Repr] = Option(params.findNode(elt, key, 0)).map(params.toT)
    def getAll(key: String): Seq[Repr] = params.findAll(elt, key).map(params.toT)
    override def newBuilder = super[Abstract].newBuilder
  }

  /** And again!
   */
  implicit def builder[V](implicit p:P0[V]) =
    new DOMPrefixTree.Builder[V, Tree[V]] {
      type Params = P0[V]
      val params:Params = p
      def newEmpty:Bld[V] { type Params=P0[V] } = builder[V](params)
      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = apply(params.buildNode(v, t))
      def apply(elt:Node):Repr = new Abstract[V](elt)
    }

  def apply[V](elt:Node):Tree[V] = new Abstract(elt)
}