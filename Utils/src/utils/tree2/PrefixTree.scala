package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IntMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer
import scala.annotation.switch

/** The standard implementation sits on Maps.
 *  This opens up some opportunities by using Map operations.
 *  Other esoteric implementation could exist, but this one should satisfy most needs, especially
 *  when it can be subclassed while keeping the same subtype for operation return (such as + etc.)
 *  See the StringTree subclass.
 */
abstract class PrefixTree[K,+V] extends PrefixTreeLike.Abstract[K,V,PrefixTree[K,V]] {  
  implicit def params:P  //make params implicit so that it is automatically used by these methods that rely on it
  def value:Option[V] = None
  def tree: Map[K,Repr]
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = bf(value,tree+kv,default)
  def -(key: K): Repr              = newBuilder(value,tree-key,default)
  def get(key: K): Option[Repr]    = tree.get(key)
  def iterator:Iterator[(K, Repr)] = tree.iterator
  /* overridden for efficiency */
  override def size: Int        = tree.size
  override def isEmpty: Boolean = tree.isEmpty
  override def foreach[U](f: ((K,Repr)) => U): Unit = tree.foreach(f)
  override def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = bf(value,tree ++ kv,default)
}

object PrefixTree extends PrefixTreeLikeBuilder.Gen2 {
  type Tree[k,+v] = PrefixTree[k, v]
  type P0[k,+v]   = Params[k,v,PrefixTree[k, v]]
  
  /** The full actual PrefixTree class used. It is designed to be sub-classed to minimize memory footprint.
   */
  protected class Abstract[K,V](implicit val params:P0[K,V]) extends PrefixTree[K, V] with super.Abstract[K,V] {
    def tree: Map[K,Repr] = params.emptyMap
  }
  
  /** The actual Parameters required to build a PrefixTree.
   */
  class Params[K,+V,+T<:Tree[K,V] with PrefixTreeLike[K,V,T]](noDefault:K=>T,stripEmpty:Boolean,mapKind:scala.collection.Map[K,T])
        extends super.Params[K,V,T](noDefault,stripEmpty) {
    def emptyMap: scala.collection.Map[K,T] = mapKind.empty
  }
  object Params {
    //the default value implies the LinkedHashMap Map implementation ; we share it.
    //The cast is OK because in this case, neither V nor K are actually used.
    protected val p0 = new Params[Any,Any,Tree[Any,Any]](PrefixTreeLikeBuilder.noElt,true,LinkedHashMap.empty[Any,Tree[Any,Any]])
    implicit def default[K,V,T<:Tree[K,V] with PrefixTreeLike[K,V,T]] = p0.asInstanceOf[Params[K,V,T]]
  }
  
  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  implicit def builder[K,V](implicit p:P0[K,V]):PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] { type P=P0[K,V] } = {
    new PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] {
      type P = P0[K,V]
      def params:P = p
      def apply(v: Option[V], t: GenTraversableOnce[(K, PrefixTree[K, V])], d: K=>PrefixTree[K, V]) = {
        val i = (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0)
        val t0 = params.emptyMap ++ t
        val t1 = if (p.stripEmpty) t0.filterNot(_._2.isNonSignificant) else t0
        (i: @switch) match {
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