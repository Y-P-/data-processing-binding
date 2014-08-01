package utils.tree1

import scala.collection.Map

/** A default implementation using the usual immutable Map as encapsulated map.
 */
abstract class MapTree[K,+V,+This<:MapTree[K,V,This]](value:Option[V],underlying:Map[K,This]) extends AbstractPrefixTree[K,V,This,MapTree](value,underlying) {this:This=>
}
object MapTree {
  class M0[K,+V](value:Option[V],underlying:Map[K,M0[K,V]]) extends MapTree[K,V,M0[K,V]](value,underlying) {
    protected[this] def builder[V1>:V,T>:M0[K,V]<:MapTree[K,V1,T]]:PrefixTreeBuilder[K,V1,T,MapTree] = null
    def selfRef:M0[K,V] = new M0(value,underlying) with super.SelfRef
    def default(key:K):M0[K,V] = ???
  }
  def builder[K,V] = new PrefixTreeBuilder[K,V,M0[K,V],MapTree] {
    def apply(value: Option[V],tree: Map[K,M0[K,V]]): M0[K,V] = new M0(value,tree)
    def emptyMap:Map[K,M0[K,V]] = Map.empty
  }
}
