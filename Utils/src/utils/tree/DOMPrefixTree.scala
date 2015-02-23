package utils.tree

import scala.collection.Map
import scala.collection.GenTraversableOnce
import org.w3c.dom.Document
import org.w3c.dom.Element

/** This provides an implementation based on DOM.
 *  This is costly to use, but provides access to advanced features from the W3 world, such as xpath or xquery.
 *  Here, keys are String and are associated to the DOM local name (no namespace used.)
 *  Nodes are stored as user data in the "" key.
 */
abstract class DOMPrefixTree[+V] protected extends PrefixTreeLike.Abstract[String,V,DOMPrefixTree[V]] {
  def elt:Element
  def value:Option[V]
  def update1[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:(String,T))(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,tree+kv,default)
  def -(key: String): Repr              = newBuilder(value,tree-key,default)
  def get(key: String): Option[Repr]    = Option(if (isAttribute(key)) elt.getAttribute(key) else elt.getElement(key))
  def iterator:Iterator[(String, Repr)] = new Iterator[(String, Repr)] {

  }
  /* overridden for efficiency */
  override def size: Int        = elt.getChildNodes.getLength
  override def isEmpty: Boolean = !elt.hasChildNodes
  override def foreach[U](f: ((String,Repr)) => U): Unit = for (i <- 0 until size) {
    val e = elt.getChildNodes.item(i)
    f(e.getLocalName,e.getUserData("").asInstanceOf[DOMPrefixTree[V]])
  }
  override def update[W>:V,T>:Repr<:PrefixTreeLike[String,W,T]](kv:GenTraversableOnce[(String,T)])(implicit bf:PrefixTreeLikeBuilder[String,W,T]): T = bf(value,tree ++ kv,default)
}

/** The PrefixTree object contains:
 *  - concrete implementations for PrefixTree
 *  - a factory for building the most appropriate implementation
 */
object DOMPrefixTree extends PrefixTreeLikeBuilder.Factory1 {
  type Tree[+v] = DOMPrefixTree[v]
  type P0[k,+v] = Params[v,DOMPrefixTree[v]]

  /** The actual Parameters required to build a PrefixTree.
   */
  class Params[+V,+T<:Tree[V] with PrefixTreeLike[String,V,T]](noDefault:String=>T,stripEmpty:Boolean)
        extends super.Params[String,V,T](noDefault,stripEmpty) {
   // def emptyMap: Map[K,T] = mapKind.empty
  }
  object Params {
    protected val p0 = new Params[Any,Tree[Any]](PrefixTreeLikeBuilder.noElt,true)
    //This is the default parameter set used.
    //The default value implies the LinkedHashMap Map implementation which preserve the iteration order.
    //We would prefer an immutable implementation but there is none.
    //The cast is OK because in this case, neither V nor K are actually used.
    //We also choose the simplest implementation: no empty node
    //You can always redefine another implicit in your scope to override this!
    implicit def default[K,V,T<:Tree[V] with PrefixTreeLike[K,V,T]] = p0.asInstanceOf[Params[V,T]]
  }

  /** The first concrete PrefixTree class.
   *  It is designed to be sub-classed to minimize memory footprint.
   *  It is public in case one wants to derive other compatible implementations.
   *  It's constructor is protected because we want users to only rely on PrefixTree[K,V] and use the factory method.
   *  params is made implicit to facilitate code writing below.
   */
  class Abstract[K,V] protected (implicit val params:P0[K,V]) extends PrefixTree[K, V] with super.Abstract[K,V] {
    def tree: Map[K,Repr] = params.emptyMap
    override def isNonSignificant = false
    override def newBuilder = super[Abstract].newBuilder
  }

  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass Abstract so as to minimize the memory footprint for each node.
   */
  implicit def builder[K,V](implicit p:P0[K,V]):PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] { type Params=P0[K,V] } = {
    new PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] {
      type Params = P0[K,V]
      val params:Params = p
      def newEmpty:PrefixTreeLikeBuilder[K,V,Repr] = builder[K,V](params)

      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = {
        val t0 = params.emptyMap ++ t
        val t1 = if (p.stripEmpty) t0.filterNot(_._2.isNonSignificant) else t0
        (if (v==None) 0x100 else 0)+(if (t1.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0: @switch) match {
          case 0x111 => new Abstract[K,V] { override def isNonSignificant = true }
          case 0x110 => new Abstract[K,V] { override val default = d }
          case 0x101 => new Abstract[K,V] { override val tree = t1 }
          case 0x100 => new Abstract[K,V] { override val tree = t1; override val default = d }
          case 0x011 => new Abstract[K,V] { override val value = v }
          case 0x010 => new Abstract[K,V] { override val value = v; override val default = d }
          case 0x001 => new Abstract[K,V] { override val value = v; override val tree = t1 }
          case 0x000 => new Abstract[K,V] { override val value = v; override val tree = t1; override val default = d }
        }
      }
    }
  }
}