package utils.tree2

import scala.collection.GenTraversableOnce

abstract class PrefixTraversable[K, +V](val value: Option[V]) extends PrefixTraversableLike[K, V, PrefixTraversable[K, V]] {
  protected[this] def newBuilder: PrefixTraversableLikeBuilder[K, V, PrefixTraversable[K, V]] = PrefixTraversable.apply
  def update1[W >: V, T >: PrefixTraversable[K, V] <: PrefixTraversableLike[K, W, T]](kv: (K, ()=>T))(implicit bf: PrefixTraversableLikeBuilder[K, W, T]): T =
    bf(value, this ++ Traversable(kv))
  override def update[W >: V, T >: PrefixTraversable[K, V] <: PrefixTraversableLike[K, W, T]](kv: (K, ()=>T)*)(implicit bf: PrefixTraversableLikeBuilder[K, W, T]): T =
    bf(value, this ++ Traversable(kv: _*))
}

object PrefixTraversable extends PrefixTraversableLikeBuilder.Gen2 {
  type Tree[k,+v] = PrefixTraversable[k,v]
  type P0[k,+v] = Null
  implicit def p[K,V]:P0[K,V] = null
  def builder[K, V](implicit p:P0[K,V]) = new PrefixTraversableLikeBuilder[K, V, PrefixTraversable[K, V]] {
    type P = Null
    def params = null
    def apply(v: Option[V], tree: GenTraversableOnce[(K, ()=>PrefixTraversable[K, V])]): PrefixTraversable[K, V] = new PrefixTraversable[K, V](v) {
      def foreach[U](f: ((K, ()=>PrefixTraversable[K,V])) => U):Unit = tree.foreach(f)
    }
    def newEmpty: PrefixTraversableLikeBuilder[K, V, PrefixTraversable[K, V]] = PrefixTraversable.builder
  }
  def apply[K, V] = builder[K,V](null)
}