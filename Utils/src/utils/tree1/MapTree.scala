package utils.tree1

import scala.collection.Map

/** A default implementation using the usual immutable Map as encapsulated map.
 */
abstract class MapTree[K,+V,+This<:MapTree[K,V,This]](value:Option[V],underlying:Map[K,This]) extends AbstractPrefixTree[K,V,This,MapTree](value,underlying) {this:This=>
  def builder[K,V,T<:MapTree[K,V,T]]:PrefixTreeBuilder[K,V,T,MapTree] = MapTree.builder
  def default(key:K):This = ???
}
object MapTree {
  class M0[K,+V](value:Option[V],underlying:Map[K,M0[K,V]]) extends MapTree[K,V,M0[K,V]](value,underlying) {
    def selfRef:M0[K,V] = new M0(value,underlying) with super.SelfRef
  }
  def builder[K,V,T<:MapTree[K,V,T]] = new PrefixTreeBuilder[K,V,T,MapTree] {
    def apply(value: Option[V],tree: Map[K,T]): T = null.asInstanceOf[T] //new M0[K,V](value,tree)
    def emptyMap:Map[K,T] = Map.empty
  }
  
  type Tree[k,+v,+F[k,+v]<:Tree[k,v,F]] = (k,F[k,v])
  var  o:Tree[String,Int,Tuple2] = null
  val x = o._1
  val y = o._2
  val z = y._2
}
