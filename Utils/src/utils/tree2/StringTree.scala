package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap

/** A common use case with String as key type.
 */
class StringTree[+V](value: Option[V],override val tree: Map[String,StringTree[V]], override val default: String=>StringTree[V]) extends PrefixTree[String,V](value,tree,default) with PrefixTreeLike[String,V,StringTree[V]] {
  protected[this] override def newBuilder:PrefixTreeLikeBuilder[String,V,Repr] = StringTree.builder[V]
}

object StringTree extends PrefixTreeLikeBuilder.GenBuilder1[String,StringTree] {
  //using LinkedHashMap as the repesentation to preserve canonical order
  implicit def builder[V] = apply[V](LinkedHashMap.empty[String, StringTree[V]])

  /** A factory for working with varied map kinds if necessary.
   *  @see LinkedPrefixTree
   */
  def apply[V](emptyMap: Map[String, StringTree[V]]):PrefixTreeLikeBuilder[String, V, StringTree[V]] =
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] { self=>
      //create a StringTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], tree: GenTraversableOnce[(String, StringTree[V])], default: String=>StringTree[V]): StringTree[V] = new StringTree[V](v, emptyMap ++ tree, default) { 
        override def newBuilder: PrefixTreeLikeBuilder[String, V, Repr] = self
      }
      override def withValue(t:StringTree[V],v:Option[V]):StringTree[V] = new StringTree(v,t.tree,t.default)
      override def withDefault(t:StringTree[V],default:String=>StringTree[V]):StringTree[V] = new StringTree(t.value,t.tree,default)
    }
}
