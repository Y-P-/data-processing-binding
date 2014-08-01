package utils.tree1

import scala.annotation.tailrec
import scala.collection.{Set,Map,GenTraversableOnce,IterableLike}
import scala.collection.generic.Subtractable

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
 *  - any child node:      K  => Option[This]       apply
 *  - if a child exists:   K  => Boolean            isDefinedAt
 *  - enumerable children:    => Boolean            isDiscrete
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
 *  @param K the key type
 *  @param V the value type, covariant
 *  @param This the actual type which a given tree respects (i.e. all nodes are garanteed
 *              to be at least of that type)
 */
trait PrefixTree[K,+V,+This<:R[K,V,This],R[k,+v,+t<:R[k,v,t]]<:PrefixTree[k,v,t,R]] extends
          PartialFunction[K, This]
     with Subtractable[K, This]
     with Iterable[(K,This)]
{ self:This=>
  //def newBuilder: scala.collection.mutable.Builder[(V,Map[K,This]), This]  
  /** the associated builder */
  protected[this] def builder[V1>:V,T>:This<:R[K,V1,T]]:PrefixTreeBuilder[K,V1,T,R]
  /** autoconvert to This - solves compiler issues - required for upper traits */
  override def repr:This = this
  /** value for this key */
  def value:Option[V]
  /** the underlying Map collection used, no default!*/
  protected[this] def underlying:Map[K,This]
  /** a default value for when the Map doesn't contain the key */
  def default(key:K): This
  
  /** indicates that this Tree node references another node */
  def isRef:Boolean = false
  /** builds a self reference -used to resolve inner links- */
  def selfRef:This
  
  /** A SelfRef forwards all calls to the referenced object which delivered the SelfRef.
   */
  trait SelfRef extends PrefixTree[K,V,This,R] {this:R[K,V,This] with This=>
    final override def isRef:Boolean = true
    final override def selfRef: This = this
  }
  
  /** We have map-like methods, either accessors, iterators or checks */
  def apply(key:K): This = underlying.get(key) match {
    case None       => default(key)
    case Some(tree) => tree
  }

  def get(key:K): Option[This]                      = underlying.get(key)
  override def isEmpty: Boolean                     = underlying.isEmpty
  def isDefinedAt(key:K): Boolean                   = underlying.isDefinedAt(key)
  def iterator: Iterator[(K, This)]                 = underlying.iterator
  def getOrElse[T >: This](key:K, default: => T): T = underlying.getOrElse(key, default)
  def contains(key:K): Boolean                      = underlying.contains(key)
  def keySet: Set[K]                                = underlying.keySet
  def keysIterator: Iterator[K]                     = underlying.keysIterator
  def keys: Iterable[K]                             = underlying.keys
  def values: Iterable[This]                        = underlying.values
  def valuesIterator: Iterator[This]                = underlying.valuesIterator
  
  /** We have the empty value, which is used to construct new Trees of the same kind */
  def updated [V1>:V,T>:This<:R[K,V1,T]](key:K, tree: T): T = builder[V1,T](value,underlying.updated(key,tree))
  def -(key:K): This = builder(value,underlying - key)
      
  /** nice way to reach an item. e.g tree->"a"->"x" looks better than t("a")("x") */
  final def ->(key:K):This = apply(key)
  
  /** subtree for this sequence of keys, without using any default */
  def get(keys:K*):Option[This] = {
    if (keys.length==0) return Some(this)
    val k = keys(0)
    val r = get(k)
    if (keys.length==1) r else {
      val r1 = r match { 
        case None    => return None
        case Some(x) => x
      }
      r1.get(keys.tail:_*)
    }
  }
  /** subtree for this sequence of keys, using all possible defaults */
  def apply(keys:K*):This = {
    if (keys.length==0) return this
    val k = keys(0)
    val r = (get(k) match {
      case None    => default(k)
      case Some(x) => x
    })
    r(keys.tail:_*)
  }
  
  /** And we have map-like methods that build new PrefixTree.
   *  Note that they don't match Map's methods signature, which is why PrefixTree cannot be a Map.
   */
  def + [V1>:V,T>:This<:R[K,V1,T]](kv: (K, T)): T =                  updated[V1,T](kv._1,kv._2)
  def ++[T>:This<:R[K,_>:V,T]](xs: GenTraversableOnce[(K, T)]): T =  xs.foldLeft[T](this)(_+_)
  def mapValues[T<:R[K,_>:V,T]](f: R[K,V,_] => T): T =               underlying.foldLeft[T](f(this))((t1,t2)=> t1.updated(t2._1,t2._2.mapValues(f)))
  def map[K1,T<:R[K1,_>:V,T]](g:K=>K1, f: This=>T): T =              underlying.foldLeft[T](f(this))((t1,t2)=> t1.updated(g(t2._1),t2._2.map[K1,T](g,f)))
  def filterKeys(p: K => Boolean): This =                            underlying.foldLeft(this)((t1,t2)=> if (p(t2._1)) t1 else t1-t2._1)
  override def filter(kv: ((K, This)) => Boolean):This =             underlying.foldLeft(this)((t1,t2)=> if (kv(t2)) t1 else t1-t2._1)  

  /** Adds or replaces T for the given sequence of keys */
  def add[V1>:V,T>:This<:R[K,V1,T]](keys:Seq[K], tree:T): T = {
    val x = keys(0)
    if (keys.length==1) this +[V1,T] ((x,tree))
    else {
      val t = get(x) match {
        case None    => throw new NullPointerException //builder.empty(None)
        case Some(m) => m 
      }
      val t1 = t.add[V1,T](keys.tail,tree)
      this +[V1,T] ((x,t1))
    }
  }
  /** Removes the value for the given sequence of keys.
   *  If the result is empty and has no value, the truncated key is removed too.
   *  This happens as long as the conditions holds walking up the sequence.
   *  Ex: this rem ("a","b","c") first removes "a"->"b"->"c"
   *      then it checks if "a"->"b" is still significant (has a proper value or non empty subtree)
   *      if not it removes it, and in that case it proceeds to "a"...
   */
  def rem (keys:K*): This = {
    if (keys.length==0) return this
    val x = keys(0)
    val m = get(x)
    if (m.isEmpty)               this
    else if (keys.length==1)     this - x
    else {
      val r = (m.get rem (keys.tail:_*))
      if (r.isEmpty && r.value.isEmpty) this rem (keys.init:_*)
      else                              repr.updated(x,r)
    }
  }
  
  /** provides a global iterator ; that iterator is used to visit the whole sub-trees */
  def seqIterator(topFirst:Boolean):Iterator[(Seq[K], This)] = new TreeIterator(Nil,topFirst)
  /** flattens the tree to it's canonical state. */
  def flatten:Seq[(Seq[K],This)] = seqIterator(true).toSeq

  /** The Tree can conveniently be viewed almost as a 'Map' with sequences of keys as key.
   *  This is very convenient to build the Tree and iterate through it.
   *  This view provides such a map-like interface.
   */
  class SeqTreeView extends Iterable[(Seq[K],This)] with PartialFunction[Seq[K], This] {
    def get(keys:Seq[K]):Option[This]                   = self.get(keys:_*)
    def +[T>:This<:R[K,_>:V,T]](kv: (Seq[K], T)): T     = (self:T).add(kv._1,kv._2)
    def -(keys: Seq[K]): This                           = self.rem(keys:_*)
    def iterator: Iterator[(Seq[K], This)]              = self.seqIterator(true)
    def apply(keys: Seq[K]):This                        = self.apply(keys:_*)
    def isDefinedAt(keys: Seq[K]):Boolean               = get(keys).isDefined
  }

  private class TreeIterator(val cur:scala.collection.immutable.List[K],topFirst:Boolean) extends Iterator[(Seq[K], This)] {
    protected[this] val iter = iterator                        //iterator for this level
    protected[this] var i:Iterator[(Seq[K], This)] = getSub    //current sub-iterator
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
        (cur,PrefixTree.this)
      } else                                                   //if the next is not the current item, then it is the current sub-ioterator next element
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
  
  /** runs through all entries which hold data.
   *  @param key, the Key for the current (this) entry
   */
  def forDefined[U](key:K)(f:(K,This)=>U):Unit = {
    if (value!=None) f(key,this)
    for (x <- iterator) x._2.forDefined(x._1)(f)
  }
  /** Tries to turn a one level deep hierarchy to a map*/
  def toMap:Map[K,V] = try {
    val m = scala.collection.mutable.HashMap.empty[K,V]
    for (v <- iterator) {
      if (!v._2.isEmpty) throw new IllegalStateException("cannot transform hierarchy to map if not exactly 1 level deep")
      m += ((v._1,v._2.value.get))
    }
    m.toMap
  } catch {
    case _:Throwable => throw new IllegalStateException("cannot transform hierarchy to a map"+this)
  }
  
  override def toString = s"${if (value==None) None else value.get}${if (!isEmpty) s" => ${underlying.toString}" else ""}"
}

trait PrefixTreeBuilder[K,V,This<:R[K,V,This],R[k,+v,+t<:R[k,v,t]]<:PrefixTree[k,v,t,R]] {
  def apply(v:Option[V],t:Map[K,This]):This  
  /** Empty map representing the underlying map class to use */
  def emptyMap:Map[K,This]
  def empty:This=apply(None,emptyMap)
}

trait CanBuildFrom0[K,V,V1>:V,This<:R[K,V,This],To<:R[K,V1,To],R[k,+v,+t<:R[k,v,t]]<:PrefixTree[k,v,t,R]] {
  def builder:PrefixTreeBuilder[K,V1,To,R]
}

/*
trait CanBuildFrom[-From, -Elem, +To] {
// Creates a new builder
def apply(from: From): Builder[Elem, To]
}
*/


