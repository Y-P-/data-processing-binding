package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap

/** A common use case with String as key type.
 */
class StringTree[+V](value: Option[V],override val tree: Map[String,StringTree[V]]) extends PrefixTree[String,V](value,tree) with PrefixTreeLike[String,V,StringTree[V]] {
  protected[this] override def newBuilder:PrefixTreeLikeBuilder[String,V,Repr] = StringTree.builder[V]
}

object StringTree extends PrefixTreeLikeBuilder.GenBuilder1[String,StringTree] {
  //using LinkedHashMap as the repesentation to preserve canonical order
  implicit def builder[V] = apply[V](LinkedHashMap.empty[String, StringTree[V]])(true)

  /** A factory for working with varied map kinds if necessary.
   *  @see LinkedPrefixTree
   */
  def apply[V](emptyMap: Map[String, StringTree[V]])(implicit replace:Boolean):PrefixTreeLikeBuilder[String, V, StringTree[V]] = {
    def b: PrefixTreeLikeBuilder[String, V, StringTree[V]] = new PrefixTreeLikeBuilder[String, V, StringTree[V]] {
      //create a StringTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], tree: GenTraversableOnce[(String, StringTree[V])]): StringTree[V] = new StringTree[V](v, emptyMap ++ tree) { 
        override def newBuilder: PrefixTreeLikeBuilder[String, V, Repr] = b
      }
    }
    b
  }
}
