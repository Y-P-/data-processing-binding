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
  def value:Option[V] = None
  def tree: Map[K,Repr] = Map.empty[K,Repr]
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

object PrefixTree extends PrefixTreeLikeBuilder.GenBuilder2[PrefixTree] {
  //note that the standard implementation uses LinkedHashMap to preserve the canonical order
  //another map kind could be used, even an immutable one
  implicit def builder[K,V] = apply(LinkedHashMap.empty[K,PrefixTree[K,V]],PrefixTreeLikeBuilder.noElt)
  
  protected class Abstract[K,V](val genericDefault:K=>Nothing) extends PrefixTree[K, V] {
    def newBuilder = PrefixTree(tree.empty,genericDefault)
    def empty      = Abstract.empty[K,V](genericDefault)
  }
  protected object Abstract {
    def empty[K,V](genericDefault:K=>Nothing) = new Abstract[K,V](genericDefault) { override def isNonSignificant = true }
  }
  
  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  def apply[K,V](emptyMap: Map[K, PrefixTree[K, V]],noDefault:K=>Nothing):PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] = {
    new PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] {
      def apply(v: Option[V], t: GenTraversableOnce[(K, PrefixTree[K, V])], d: K=>PrefixTree[K, V]) = {
        val i = (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0)
        (i: @switch) match {
          case 0x111 => Abstract.empty[K,V](noDefault)
          case 0x110 => new Abstract[K,V](noDefault) { override val default = d }
          case 0x101 => new Abstract[K,V](noDefault) { override val tree = emptyMap ++ t }
          case 0x100 => new Abstract[K,V](noDefault) { override val tree = emptyMap ++ t; override val default = d }
          case 0x011 => new Abstract[K,V](noDefault) { override val value = v }
          case 0x010 => new Abstract[K,V](noDefault) { override val value = v; override val default = d }
          case 0x001 => new Abstract[K,V](noDefault) { override val value = v; override val tree = emptyMap ++ t }
          case 0x000 => new Abstract[K,V](noDefault) { override val value = v; override val tree = emptyMap ++ t; override val default = d }
        }
      }
    }
  }
}