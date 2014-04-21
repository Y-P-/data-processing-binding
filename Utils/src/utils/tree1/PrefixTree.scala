package utils.tree1

import scala.annotation.tailrec
import scala.collection.{Set,Map,GenTraversableOnce,IterableLike}
import scala.collection.generic.Subtractable

trait PrefixTree[K,+V,+This<:R[K,V,This],R[k,+v,+t<:R[k,v,t]]<:PrefixTree[k,v,t,R]] extends
          PartialFunction[K, This]
     with Subtractable[K, This]
     with Iterable[(K,This)]
{ self:R[K,V,This] with This=>
  type Next[+t] = R[K,V,t] with t
  /** the associated builder */
  def builder[K,V,T<:R[K,V,T]]:PrefixTreeBuilder[K,V,T,R]
  /** autoconvert to This - solves compiler issues - required for upper traits */
  override def repr:This = this
  /** value for this key */
  def value:Option[V]
  /** the underlying Map collection used, no default!*/
  protected[this] def underlying:Map[K,Next[_]]
  /** a default value for when the Map doesn't contain the key */
  def default(key:K): This
  
  /** indicates that this Tree node references another node */
  def isRef:Boolean = false
  /** builds a self reference -used to resolve inner links- */
  def selfRef:This
  
  /** A SelfRef forwards all calls to the referenced object which delivered the SelfRef.
   */
  trait SelfRef extends PrefixTree[K,V,This,R] {this:This=>
    final override def isRef:Boolean = true
    final override def selfRef: This = this
  }
  
  /** We have map-like methods, either accessors, iterators or checks */
  def apply(key:K): Next[_] = underlying.get(key) match {
    case None       => default(key)
    case Some(tree) => tree
  }

  def get(key:K): Option[Next[_]]                  = underlying.get(key)
  override def isEmpty: Boolean                     = underlying.isEmpty
  def isDefinedAt(key:K): Boolean                   = underlying.isDefinedAt(key)
  def iterator: Iterator[(K, Next[_])]             = underlying.iterator
  def getOrElse[T >: Next[_]](key:K, default: => T): T = underlying.getOrElse(key, default)
  def contains(key:K): Boolean                      = underlying.contains(key)
  def keySet: Set[K]                                = underlying.keySet
  def keysIterator: Iterator[K]                     = underlying.keysIterator
  def keys: Iterable[K]                             = underlying.keys
  def values: Iterable[Next[_]]                    = underlying.values
  def valuesIterator: Iterator[Next[_]]            = underlying.valuesIterator
  
  /** We have the empty value, which is used to construct new Trees of the same kind */
  def updated [V1>:V,T>:Next[_]<:R[K,V1,T]](key:K, tree: T): Next[T] = builder(value,underlying.updated[T](key,tree))
  def -(key:K): This = builder[K,V,This](value,underlying - key)
      
  /** nice way to reach an item. e.g tree->"a"->"x" looks better than t("a")("x") */
  final def ->(key:K):Next[_] = apply(key)
  
  /** subtree for this sequence of keys, without using any default */
  def get(keys:K*):Option[Next[_]] = {
    if (keys.length==0) return Some(this)
    val k = keys(0)
    val r = get(k)
    if (keys.length==1) r else (r match {
      case None    => return None
      case Some(x) => x
    }).get(keys.tail:_*)
  }
  /** subtree for this sequence of keys, using all possible defaults */
  def apply(keys:K*):Next[_] = {
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
  def + [V1>:V,T>:Next[_]<:R[K,V1,T]](kv: (K, T)): Next[T] =
    updated[V1,T](kv._1,kv._2)
  def ++[V1>:V,T>:Next[_]<:R[K,V1,T]](xs: GenTraversableOnce[(K, T)]): Next[T] =
    xs.foldLeft(this)(_+[V1,T]_)
  def mapValues[V1>:V,T<:R[K,V1,T]](f: This => T): T =
    underlying.foldLeft[T](f(this))((t1,t2)=> t1.updated(t2._1,t2._2.mapValues[V1,T](f)))
  def map[K1,V1,T<:R[K1,V1,T]](g:K=>K1, f: Next[_]=>T): T =
    underlying.foldLeft[T](f(this))((t1,t2)=> t1.updated(g(t2._1),t2._2.map[K1,V1,T](g,f)))
  def filterKeys(p: K => Boolean): This =
    underlying.foldLeft(this)((t1,t2)=> if (p(t2._1)) t1 else t1-t2._1)
  override def filter(kv: ((K, Next[_])) => Boolean):This =
    underlying.foldLeft(this)((t1,t2)=> if (kv(t2)) t1 else t1-t2._1)  

  /** Adds or replaces T for the given sequence of keys */
  def add[V1>:V,T>:Next[_]<:R[K,V1,T]](keys:Seq[K], tree:T): T = {
    val x = keys(0)
    if (keys.length==1) (this:T) + ((x,tree))
    else {
      val t:T = get(x) match {
        case None    => builder.empty(None)
        case Some(m) => m.repr 
      }
      val t1:T = t.add[V1,T](keys.tail,tree)
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
  def seqIterator(topFirst:Boolean):Iterator[(Seq[K], Next[_])] = new TreeIterator(Nil,topFirst)
  /** flattens the tree to it's canonical state. */
  def flatten:Seq[(Seq[K],Next[_])] = seqIterator(true).toSeq

  /** The Tree can conveniently be viewed almost as a 'Map' with sequences of keys as key.
   *  This is very convenient to build the Tree and iterate through it.
   *  This view provides such a map-like interface.
   */
  class SeqTreeView extends Iterable[(Seq[K],Next[_])] with PartialFunction[Seq[K], This] {
    def get(keys:Seq[K]):Option[Next[_]]               = self.get(keys:_*)
    def +[V1>:V,T>:This<:R[K,V1,T]](kv: (Seq[K], T)): T = (self:T).add(kv._1,kv._2)
    def -(keys: Seq[K]): This                           = self.rem(keys:_*)
    def iterator: Iterator[(Seq[K], Next[_])]          = self.seqIterator(true)
    def apply(keys: Seq[K]):Next[_]                    = self.apply(keys:_*)
    def isDefinedAt(keys: Seq[K]):Boolean               = get(keys).isDefined
  }

  private class TreeIterator(val cur:scala.collection.immutable.List[K],topFirst:Boolean) extends Iterator[(Seq[K], Next[_])] {
    protected[this] val iter = iterator                        //iterator for this level
    protected[this] var i:Iterator[(Seq[K], Next[_])] = getSub//current sub-iterator
    protected[this] var done:Boolean = false                   //true when this item has been provided
    @tailrec final def hasNext:Boolean = {
      if (!done)     return true                               //if this item has not been processed, there is a next
      if (i==null)   return false                              //if there is no sub-iterator available (this item neing processed), we are finished
      if (i.hasNext) return true                               //but if there is a sub-iterator with a next element, then there is a next
      i = getSub                                               //for self recursing trees, we must find here if we can go on, i.e. fetch the next sub-iterator
      hasNext                                                  //and then check if it has a next element
    }
    def next(): (Seq[K], Next[_]) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,PrefixTree.this)
      } else                                                   //if the next is not the current item, then it is the current sub-ioterator next element
        i.next
    }
    def getSub:Iterator[(Seq[K], Next[_])] = {
      if (iter.hasNext) {                                      //move to next element
        val (k,t:PrefixTree[K,V,_,R])=iter.next
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
  def forDefined[U](key:K)(f:(K,Next[_])=>U):Unit = {
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
  type Next[+t] = R[K,V,t] with t
  def apply(v:Option[V],t:Map[K,Next[_]]):Next[This]  
  /** Empty map representing the underlying map class to use. LinkedHashMap by default */
  def emptyMap:Map[K,Next[_]]
  def empty:This=apply(None,emptyMap)
}
