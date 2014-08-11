package utils.tree2
/*
import scala.annotation.tailrec
import scala.collection.{Set,Map,GenTraversableOnce,IterableLike}
import scala.collection.generic.Subtractable
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.Builder
import java.util.NoSuchElementException
import scala.collection.immutable.HashMap
import scala.collection.Map
import scala.collection.MapLike
import scala.collection.immutable.AbstractMap

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
trait PrefixTreeLike[K,+V,+This<:PrefixTreeLike[K,V,This]] extends Map[K,This] with MapLike[K,This,This] { self:This=>
  /** no-cast conversion to This - solves compiler issues - required for upper traits */
  override def repr:This = this
  //override protected[this] def newBuiler:Builder[(K, This),This] = null
  override def empty:This = null.asInstanceOf[This]
  /** the way to reach a given child for its value and sub-tree ; default values, if any, must be accounted for here */
  //@throws(classOf[NoSuchElementException])
  //def get:PartialFunction[K, This]
  /** nice way to reach an item. e.g tree->"a"->"x" looks better than t("a")("x") */
  //def ->(key:K):This = apply(key)
  /** indicates if this is a reference to a tree -as opposed to a real tree- */
  def isRef:Boolean = false
  /** value for that node, if any */
  val value:Option[V]

  /** subtree for this sequence of keys, using all possible defaults */
  @tailrec def deepGet(keys:K*):Option[This] = {
    if (keys.length==0) throw new IllegalArgumentException
    val r = get(keys(0))
    if (keys.length==1) r  else r.get.deepGet(keys.tail:_*)
  }
  @tailrec def isDefinedAt(keys:K*):Boolean = {
    if (keys.length==0) throw new IllegalArgumentException
    val r = get(keys(0))
    if (keys.length==1) r.isDefined  else r.get.isDefinedAt(keys.tail:_*)
  }
    
  /** provides a global iterator ; that iterator is used to visit the whole sub-trees */
  def seqIterator(topFirst:Boolean):Iterator[(Seq[K], This)] = new TreeIterator(Nil,topFirst)
  
  private class TreeIterator(val cur:scala.collection.immutable.List[K],topFirst:Boolean) extends Iterator[(Seq[K], This)] {
    protected[this] val iter = iterator                        //iterator for this level
    protected[this] var i:Iterator[(Seq[K], This)] = getSub     //current sub-iterator
    protected[this] var done:Boolean = false                   //true when this item has been provided
    @tailrec final def hasNext:Boolean = {
      if (!done)     return true                               //if this item has not been processed, there is a next
      if (i==null)   return false                              //if there is no sub-iterator available (this item neing processed), we are finished
      if (i.hasNext) return true                               //but if there is a sub-iterator with a next element, then there is a next
      i = getSub                                               //for self recursing trees, we must find here if we can go on, i.e. fetch the next sub-iterator
      hasNext                                                  //and then check if it has a next element
    }
    def next(): (Seq[K], This) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,repr)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    def getSub:Iterator[(Seq[K], This)] = {
      if (iter.hasNext) {                                      //move to next element
        val (k,t)=iter.next
        if (!t.isRef)                                          //if not already processed
          new t.TreeIterator(cur.+:(k),topFirst)               //fetch sub-iterator
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
  class SeqTreeView extends Map[Seq[K],This] with MapLike[Seq[K],This,SeqTreeView] {
    def get(keys:Seq[K]):Option[This]                = self.deepGet(keys:_*)
    def +[B1 >: This](kv: (Seq[K], B1)): SeqTreeView = null //(self:B1).add(kv._1,kv._2)
    def -(keys: Seq[K]): SeqTreeView               = null //self.rem(keys:_*)
    def iterator: Iterator[(Seq[K], This)]         = self.seqIterator(true)
    override def empty: SeqTreeView                = null
    override def isDefinedAt(keys: Seq[K]):Boolean = self.isDefinedAt(keys:_*) 
  }
  
}

class PrefixTree[K,+V](val value: Option[V],val tree: Map[K,PrefixTree[K,V]]) extends AbstractMap[K,PrefixTree[K,V]] with PrefixTreeLike[K,V,PrefixTree[K,V]] {
  def +[B1 >: PrefixTree[K,V]](kv: (K, B1)): PrefixTree[K,V] = ???
  def -(key: K): PrefixTree[K,V]                             = new PrefixTree(value,tree-key)
  def get(key: K): Option[PrefixTree[K,V]]                   = tree.get(key)
  def iterator:Iterator[(K, PrefixTree[K,V])]                = tree.iterator
  override def empty: PrefixTree[K,V]                        = new PrefixTree[K,V](None,tree.empty)
}

object PrefixTree {
  def apply[K,V](v:Option[V],tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V] = new PrefixTree[K,V](v,tree)
  def apply[K,V](v:V,tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V]         = apply(Some(v),tree)
  def apply[K,V](tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V]             = apply(None,tree)
}

class StringTree[+V](value: Option[V],tree: Map[String,StringTree[V]]) extends PrefixTree[String,V](value,tree) {
  override def + [B1 >: StringTree[V]](kv: (String, B1))     = null //new StringTree(value,tree + kv)
  override def -(key: String): StringTree[V]                 = new StringTree(value,tree - key)
  override def get(key: String): Option[StringTree[V]]       = tree.get(key)
  override def iterator: Iterator[(String, StringTree[V])]   = tree.iterator
  override def empty: StringTree[V]                          = new StringTree[V](None,tree.empty)
}

*/