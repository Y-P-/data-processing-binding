package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IntMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

/** The standard implementation sits on Maps.
 *  This opens up some opportunities by using Map operations.
 *  Other esoteric implementation could exist, but this one should satisfy most needs, especially
 *  when it can be subclassed while keeping the same subtype for operation return (such as + etc.)
 *  See the StringTree subclass.
 */
class PrefixTree[K,+V](val value:Option[V], val tree: Map[K,PrefixTree[K,V]], override val default:K=>PrefixTree[K,V]) extends PrefixTreeLike.Abstract[K,V,PrefixTree[K,V]] {
  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr] = PrefixTree.builder[K,V]
  
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =  bf(value,tree+kv,default)
  def -(key: K): Repr              = newBuilder(value,tree-key,default)
  def get(key: K): Option[Repr]    = tree.get(key)
  def iterator:Iterator[(K, Repr)] = tree.iterator
  /* overridden for efficiency */
  override def size: Int        = tree.size
  override def isEmpty: Boolean = tree.isEmpty
  override def foreach[U](f: ((K,Repr)) => U): Unit = tree.foreach(f)
}

object PrefixTree extends PrefixTreeLikeBuilder.GenBuilder2[PrefixTree] {
  //note that the standard implementation uses LinkedHashMap to preserve the canonical order
  //another map kind could be used, even an immutable one
  implicit def builder[K,V] = apply(LinkedHashMap.empty[K,PrefixTree[K,V]])
  
  /** A factory for working with varied map kinds if necessary.
   *  @see LinkedPrefixTree
   */
  def apply[K,V](emptyMap: Map[K, PrefixTree[K, V]]):PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] =
    new PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] { self=>
      //create a PrefixTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], tree: GenTraversableOnce[(K, PrefixTree[K, V])], default: K=>PrefixTree[K, V]): PrefixTree[K, V] = new PrefixTree[K, V](v, emptyMap ++ tree, default) { 
        override def newBuilder: PrefixTreeLikeBuilder[K, V, Repr] = self
      }
      override def withValue(t:PrefixTree[K, V],v:Option[V]):PrefixTree[K, V] = new PrefixTree(v,t.tree,t.default)
      override def withDefault(t:PrefixTree[K, V],default:K=>PrefixTree[K, V]):PrefixTree[K, V] = new PrefixTree(t.value,t.tree,default)
    }
  
}