package utils.tree

/** A stub where the implementation relies on an encapsulated Map.
 *  The exact Map implementation is left to the user.
 *  This is a pretty standard implementation for MapTree without resorting to writing ones own Map.
 *  It is mutable.
 *  Also, as an abstract class, it may prevent code bloating.
 */
abstract class AbstractDelegatedMapTree[K,V,This<:AbstractDelegatedMapTree[K,V,This]](val value:Option[V]) extends MapTreeLike[K,V,This] with Cloneable {this:This=>
  protected var self:scala.collection.Map[K,This]
  def get(key:K):Option[This]         = self.get(key)
  def iterator: Iterator[(K, This)]   = self.iterator
  def +=(kv: (K, This)): this.type    = { self+=kv; this }
  def -=(key: K): this.type           = { self-=key; this }
  override def isEmpty:Boolean        = self.isEmpty
  override def clone:This = {
    val r = super.clone.asInstanceOf[This]
    if (self.isInstanceOf[scala.collection.mutable.Map[K,This]])
      r.self = self.asInstanceOf[scala.collection.mutable.Map[K,This]].clone
    r
  }
}
