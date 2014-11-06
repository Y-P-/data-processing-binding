package utils.tree2

import scala.collection.GenTraversableOnce

abstract class PrefixIterable[K, +V](val value: Option[V]) extends PrefixIterableLike[K, V, PrefixIterable[K, V]] {
  protected[this] def newBuilder: PrefixIterableLikeBuilder[K, V, PrefixIterable[K, V]] = PrefixIterable.apply
  def update1[W >: V, T >: PrefixIterable[K, V] <: PrefixIterableLike[K, W, T]](kv: (K, T))(implicit bf: PrefixIterableLikeBuilder[K, W, T]): T =
    bf(value, iterator ++ Iterator.single(kv))
  override def update[W >: V, T >: PrefixIterable[K, V] <: PrefixIterableLike[K, W, T]](kv: (K, T)*)(implicit bf: PrefixIterableLikeBuilder[K, W, T]): T =
    bf(value, iterator ++ Iterator(kv: _*))
}

object PrefixIterable extends PrefixIterableLikeBuilder.Gen2 {
  type Tree[k,+v] = PrefixIterable[k,v]
  type P0[k,+v] = Null
  implicit def p[K,V]:P0[K,V] = null
  def builder[K, V](implicit p:P0[K,V]) = new PrefixIterableLikeBuilder[K, V, PrefixIterable[K, V]] {
    type P = Null
    def params = null
    def apply(v: Option[V], tree: GenTraversableOnce[(K, PrefixIterable[K, V])]): PrefixIterable[K, V] = new PrefixIterable[K, V](v) {
      def iterator = tree.toIterator
    }
    def newEmpty: PrefixIterableLikeBuilder[K, V, PrefixIterable[K, V]] = PrefixIterable.builder
  }
  def apply[K, V] = builder[K,V](null)
}