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
abstract class PrefixTree[K,+V] extends PrefixTreeLike.Abstract[K,V,PrefixTree[K,V]] {  
  def value:Option[V] = None
  def tree: Map[K,Repr] = Map.empty[K,Repr]
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
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  def apply[K,V](emptyMap: Map[K, PrefixTree[K, V]]):PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] = {
    new PrefixTreeLikeBuilder[K, V, PrefixTree[K, V]] { self=>
      class Abstract extends PrefixTree[K, V] {
        def newBuilder: PrefixTreeLikeBuilder[K, V, Repr] = self
        def default = noDefault
      }
      //create a PrefixTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], t: GenTraversableOnce[(K, PrefixTree[K, V])], d: K=>PrefixTree[K, V]) = (v==None,t.isEmpty,d==null) match {
        case (true,true,true)    => new Abstract
        case (true,true,false)   => new Abstract { override val default = d }
        case (true,false,true)   => new Abstract { override val tree = emptyMap ++ t }
        case (true,false,false)  => new Abstract { override val tree = emptyMap ++ t; override val default = d }
        case (false,true,true)   => new Abstract { override val value = v }
        case (false,true,false)  => new Abstract { override val value = v; override val default = d }
        case (false,false,true)  => new Abstract { override val value = v; override val tree = emptyMap ++ t }
        case (false,false,false) => new Abstract { override val value = v; override val tree = emptyMap ++ t; override val default = d }
      }
    }
  }
  
}