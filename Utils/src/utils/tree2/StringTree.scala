package utils.tree2

import scala.collection.GenTraversableOnce

/** A common use case with String as key type.
 */
class StringTree[+V](value: Option[V],override val tree: Map[String,StringTree[V]]) extends PrefixTree[String,V](value,tree) with PrefixTreeLike[String,V,StringTree[V]] {
  protected[this] override def newBuilder:PrefixTreeLikeBuilder[String,V,Repr] = StringTree.builder[V]
}

object StringTree {
  implicit def builder[V] = new PrefixTreeLikeBuilder[String,V,StringTree[V]](new StringTree[V](None,Map.empty[String,StringTree[V]])) {
    def apply(v: Option[V], tree: GenTraversableOnce[(String,StringTree[V])]):StringTree[V] = new StringTree[V](v,empty.tree++tree)
  }
  def apply[V](v:Option[V],tree:Map[String,StringTree[V]]):StringTree[V] = builder(v,tree)
  def apply[V](v:V,tree:Map[String,StringTree[V]]):StringTree[V]         = apply(Some(v),tree)
  def apply[V](tree:Map[String,StringTree[V]]):StringTree[V]             = apply(None,tree)
  def apply[V](v:V):StringTree[V]                                        = apply(Some(v),Map.empty[String,StringTree[V]])
}
