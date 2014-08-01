package utils.tree2

import scala.annotation.tailrec
import scala.collection.{Set,Map,GenTraversableOnce,IterableLike}
import scala.collection.generic.Subtractable
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.Builder
import java.util.NoSuchElementException
import scala.collection.immutable.HashMap

/** A Prefix Tree is a tree where each sub-node has a name (K) and possibly a value (V)
 *  A child node is reached through its name (K)
 *  The name is not intrinsic (i.e. not carried by the node itself), but rather extrinsic
 *  and carried by its parent (so the tree structure can be moved around and renamed.)
 *  Two kinds of Nodes do exist:
 *  - These where the number of children can be enumerated, and these where this is not
 *    possible. The first case is often implemented as a Map[K,This], while the latter
 *    is implemented as a PartialFunction[K,This]. This is fine because a map also is
 *    a PartialFunction[K,This].
 *  - its children names when this has a meaning (i.e. they are enumerable, possibly
 *    infinite.)
 *  
 *  From a given Node nd, one can get:
 *  - its value               => Option[V]          value
 *  - any child node:      K  => This               apply
 *  - if a child exists:   K  => Boolean            isDefinedAt
 *  - children names:         => Option[Stream[K]]  children
 *  
 *  This implementation considers nodes as immutable: hence it wants covariance on
 *  both K and This (actual tree kind)
 *  
 *  Note that there is nothing that should prevent a sub-node of a given tree from
 *  referencing a super-node, actually creating infinite depth trees. The simplest
 *  example would be t = [ v, (k) -> t ] where all keys combinations yield the
 *  constant tree t and associated value v.
 *  
 *  @param K    the key type
 *  @param V    the value type, covariant
 */
trait PrefixTree[K,+V] { self=>
  /** A type which is warranted to represent all sub-nodes of the tree */
  type Sup>:this.type<:PrefixTree.PF[K,V,Sup]
  /** no-cast conversion to Sup - solves compiler issues - required for upper traits */
  final def repr:Sup = this
  /** the canonical representation, if the (non default) children can be enumerated */
  def canonical:Option[Iterable[(K,(Option[V],Sup))]]
  /** the way to reach a given child for its value and sub-tree ; default values, if any, must be accounted for here */
  @throws(classOf[NoSuchElementException])
  def value:PartialFunction[K, (Option[V],Sup)]
  /** nice way to reach an item. e.g tree->"a"->"x" looks better than t("a")("x") */
  def ->(key:K):(Option[V],Sup) = value(key)
  
  /** subtree for this sequence of keys, using all possible defaults */
  def apply(keys:K*):(Option[V],Sup) = {
    if (keys.length==0) throw new IllegalArgumentException
    val r = value(keys(0))
    if (keys.length==1) r  else r._2.apply(keys.tail:_*)
  }
  
  /** an iterator on the defined (non default) elements */
  final def iterator: Iterator[(K, (Option[V],Sup))] = canonical match {
    case None    => null
    case Some(i) => i.toIterator
  }
  
  private class TreeIterator(val cur:scala.collection.immutable.List[K],topFirst:Boolean) extends Iterator[(Seq[K], (Option[V],Sup))] {
    protected[this] val iter = iterator                                   //iterator for this level
    protected[this] var i:Iterator[(Seq[K], (Option[V],Sup))] = getSub    //current sub-iterator
    protected[this] var done:Boolean = false                              //true when this item has been provided
    @tailrec final def hasNext:Boolean = {
      if (!done)     return true                               //if this item has not been processed, there is a next
      if (i==null)   return false                              //if there is no sub-iterator available (this item neing processed), we are finished
      if (i.hasNext) return true                               //but if there is a sub-iterator with a next element, then there is a next
      i = getSub                                               //for self recursing trees, we must find here if we can go on, i.e. fetch the next sub-iterator
      hasNext                                                  //and then check if it has a next element
    }
    def next(): (Seq[K], (Option[V],Sup)) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,repr)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    def getSub:Iterator[(Seq[K], (Option[V],Sup))] = {
      if (iter.hasNext) {                                      //move to next element
        val (k,t)=iter.next
        if (!t.isRef)                                          //if not already processed
          new t._2.TreeIterator(cur.+:(k),topFirst)            //fetch sub-iterator
        else {
          Iterator((cur.+:(k),t))                              //iterate superficially on self-references (otherwise you might get an infinite loop)
        }
      } else
        null                                                   //return null when finished
    }
  }
  
  /** The Tree can conveniently be viewed almost as a 'Map' with sequences of keys as key.
   *  This is very convenient to build the Tree and iterate through it.
   *  This view provides such a map-like interface.
   */
  class SeqTreeView extends Iterable[(Seq[K],Sup)] with PartialFunction[Seq[K], Sup] {
    def get(keys:Seq[K]):(Option[V],Sup)             = self(keys:_*)
    def +[T>:This<:R[K,_>:V,T]](kv: (Seq[K], T)): T     = (self:T).add(kv._1,kv._2)
    def -(keys: Seq[K]): This                           = self.rem(keys:_*)
    def iterator: Iterator[(Seq[K], This)]              = self.seqIterator(true)
    def apply(keys: Seq[K]):This                        = self.apply(keys:_*)
    def isDefinedAt(keys: Seq[K]):Boolean               = get(keys).isDefined
  }
  
}
object PrefixTree {
  type PF[K,+V,+S]=PrefixTree[K,V] { type Sup<:S }
}


/**
 * An implementation of PrefixTree where the key/values are stored in a Map.
 * Note that maps conveniently default to a method : this container can thus be used with
 * some generality.
 */
trait MapPrefixTree[K,+V] extends PrefixTree[K,V] {
  /** Implementation */
  final def canonical:Option[Iterable[(K,(Option[V],Sup))]] = Some(value)
  def value:Map[K,(Option[V],Sup)] 
}
object MapPrefixTree {
  def apply[K,V,S>:MapPrefixTree[K,V]<:PrefixTree.PF[K,V,S]](map:Map[K,(Option[V],S)]) = new MapPrefixTree[K,V] {
    type Sup = S
    val value = map
  }
}
