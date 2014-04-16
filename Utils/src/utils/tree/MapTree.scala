package utils.tree

/** A default implementation using the usual immutable Map as encapsulated map.
 */
class MapTree[K,V](value:Option[V]) extends AbstractDelegatedMapTree[K,V,MapTree[K,V]](value) {
  def this(value:Option[V],tree:scala.collection.Map[K,MapTree[K,V]]) = {
    this(value)
    self = tree
  }
  override def empty: MapTree[K,V] = new MapTree(None)
  protected var self:scala.collection.Map[K,MapTree[K,V]] = Map.empty
}
