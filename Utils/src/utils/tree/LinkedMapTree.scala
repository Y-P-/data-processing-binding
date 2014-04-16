package utils.tree


/** A default implementation using the usual LinkedHashMap as encapsulated map.
 *  This preserves the reading order but has a higher memory footprint.
 */
class LinkedMapTree[K,V](value:Option[V]) extends AbstractDelegatedMapTree[K,V,LinkedMapTree[K,V]](value) {
  def this(value:Option[V],tree:scala.collection.Map[K,LinkedMapTree[K,V]]) = {
    this(value)
    self = tree
  }
  override def empty: LinkedMapTree[K,V] = new LinkedMapTree(None)
  protected var self:scala.collection.Map[K,LinkedMapTree[K,V]] = scala.collection.mutable.LinkedHashMap.empty
}
