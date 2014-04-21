package utils.tree1

import scala.collection.Map

/** A stub where the implementation relies on an encapsulated Map.
 *  The exact Map implementation is left to the user.
 *  This is a pretty standard implementation for MapTree without resorting to writing ones own Map.
 *  It is mutable.
 *  Also, as an abstract class, it may prevent code bloating.
 */
abstract class AbstractPrefixTree[K,+V,+This<:R[K,V,This],R[k,+v,+t<:R[k,v,t]]<:AbstractPrefixTree[k,v,t,R]](val value:Option[V], protected[this] var underlying:Map[K,This]) extends PrefixTree[K,V,This,R] {this:This=>
}
