package utils.tree

import scala.collection.Map
import scala.collection.GenTraversableOnce
import scala.collection.mutable.LinkedHashMap
import scala.annotation.switch

/** This provides the standard implementation for PrefixTreeLike. not surprisingly, it sits on Maps.
 *  This opens up some opportunities by using Map operations.
 *  Other implementation could exist, but this one should satisfy most needs, especially
 *  when it can be subclassed while keeping the same subtype for operation return (such as + etc.)
 *  See the StringTree subclass in the test code.
 *
 *  Note that the underlying kind of tree is undefined yet.
 *  It mostly matters for either performance or iteration order.
 *
 *  Note that here, we just follow the PrefixTreeLike guideline for creating an implementation.
 */
abstract class PrefixTree[K,+V] protected extends PrefixTreeLike.Abstract[K,V,PrefixTree[K,V]] {
  def value:Option[V] = None
  def tree: Map[K,Repr]
  def update1[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = bf(value,tree+kv,default)
  def -(key: K): Repr              = newBuilder(value,tree-key,default)
  def get(key: K): Option[Repr]    = tree.get(key)
  def iterator:Iterator[(K, Repr)] = tree.iterator
  /* overridden for efficiency */
  override def size: Int        = tree.size
  override def isEmpty: Boolean = tree.isEmpty
  override def foreach[U](f: ((K,Repr)) => U): Unit = tree.foreach(f)
  override def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = bf(value,tree ++ kv,default)
}

/** The PrefixTree object contains:
 *  - concrete implementations for PrefixTree
 *  - a factory for building the most appropriate implementation
 */
object PrefixTree extends PrefixTreeLikeBuilder.Factory2 {
  type Tree[k,+v] = PrefixTree[k, v]
  type P0[k,+v]   = Params[k,v,PrefixTree[k, v]]
  type Bld[k,v]   = PrefixTreeLikeBuilder[k, v, PrefixTree[k, v]]

  /** The actual Parameters required to build a PrefixTree.
   */
  class Params[K,+V,+T<:Tree[K,V] with PrefixTreeLike[K,V,T]](noDefault:K=>T,stripEmpty:Boolean,mapKind:Map[K,T])
        extends super.Params[K,V,T](noDefault,stripEmpty) {
    def emptyMap: Map[K,T] = mapKind.empty
  }
  object Params {
    protected val p0 = new Params[Any,Any,Tree[Any,Any]](PrefixTreeLikeBuilder.noElt,true,LinkedHashMap.empty[Any,Tree[Any,Any]])
    //This is the default parameter set used.
    //The default value implies the LinkedHashMap Map implementation which preserve the iteration order.
    //We would prefer an immutable implementation but there is none.
    //The cast is OK because in this case, neither V nor K are actually used.
    //We also choose the simplest implementation: no empty node
    //You can always redefine another implicit in your scope to override this!
    implicit def default[K,V,T<:Tree[K,V] with PrefixTreeLike[K,V,T]] = p0.asInstanceOf[Params[K,V,T]]
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
    override def newBuilder:Bld[K, V] { type Params=P0[K,V] } = super[Abstract].newBuilder
  }

  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass Abstract so as to minimize the memory footprint for each node.
   */
  implicit def builder[K,V](implicit p:P0[K,V]):Bld[K, V] { type Params=P0[K,V] } = {
    new PrefixTreeLikeBuilder[K, V, Tree[K, V]] {
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