package utils.tree

import scala.annotation.tailrec


/** A Tree is an association of key/values.
 *  Unlike Maps, this association goes "in depth".
 *  For example:
 *     t: TreeBase[String,Int]
 *     val t1 = t -> "a" -> "b" -> "c"   //fetches subtree for the composite key a.b.c
 *     val v1 = t1.value                 //fetches the value for the composite key a.b.c
 *     val t2 = t1 -> "d"                //fetches subtree for the composite key a.b.c.d
 *     val v2 = t2.value                 //fetches the value for the composite key a.b.c.d
 *  This provides an immutable interface.
 *  For performance reasons, it is dubious that there is a real interest in a real immutable implementation.
 *  An immutable view is thus usually preferred and this will provide it.
 *  
 *  @param K, the type of Key, which are homogeneous at all levels
 *  @param V, the type of Value, which are homogeneous at all levels
 *  @param This, the upper limit of all contained sub-trees
 */
trait MapTreeLikeImmutable[K,+V,+This<:MapTreeLikeImmutable[K,V,This]] extends Iterable[(K,This)] {self:This=>
  final def myself:This = this
  /** value for this key */
  def value:Option[V]
  /** indicates that this Tree node references another node */
  def isRef:Boolean = false
  
  //All of the following methods have equivalence in Map and are shown to remind the user about what needs be implemented.
  /** subtree for this key */
  def get(key:K):Option[This]
  /** iterator */
  def iterator: Iterator[(K, This)]
  /** add element */
  //def +[T>:This<:MapTreeLikeImmutable[K,V,T]](kv:(K,T)):T
  protected[this] def += (kv: (K, This)): this.type
  /** remove element */
  //def -(key:K):This
  protected[this] def -=(key: K): this.type
  /** tells if the subtree is empty */
  def isEmpty:Boolean
  /** empty implementation */
  def empty: This
  /** build a self reference -used to resolve inner links- */
  def selfRef:SelfRef
  
  trait SelfRef extends MapTreeLikeImmutable[K,V,This] {this:This=>
    final def owner:This = self
    final override def isRef:Boolean  = true
  }

  /** default value ; there is a default implementation that throws an exception. You may want to override this if the Tree is not exactly a Map... */
  def default(key:K):This
  def apply(key:K) = get(key) match {
    case None    => default(key)
    case Some(v) => v
  }
  
  //local implementations
  /** nice way to reach an item. e.g tree->"a"->"x" looks better than t("a")("x") */
  final def ->(key:K):This = apply(key)
  
  //methods on sequences of keys
  /** Adds or replaces T for the given sequence of keys */
  protected[this] def add(keys:Seq[K], tree:This): This

  /** Removes the value for the given sequence of keys.
   *  If the result is empty and has no value, the truncated key is removed too.
   *  This happens as long as the conditions holds walking up the sequence.
   *  Ex: this rem ("a","b","c") first removes "a"->"b"->"c"
   *      then it checks if "a"->"b" is still significant (has a proper value or non empty subtree)
   *      if not it removes it, and in that case it proceeds to "a"...
   */
  def rem (keys:K*): This = {
    if (keys.length==1) return this
    val x = keys(0)
    val m = get(x)
    if (keys.length==1 || m.isEmpty)    this -= x
    else {
      val r = m.get rem (keys.tail:_*)
      if (r.isEmpty && r.value.isEmpty) this rem (keys.init:_*)
      else                              this += ((x,r))
    }
  }
  /** subtree for this sequence of keys, without using any default */
  def get(keys:K*):Option[This] = {
    if (keys.length==0) return Some(this)
    val k = keys(0)
    val r = get(k)
    if (keys.length==1) r else (r match {
      case None    => return None
      case Some(x) => x
    }).get(keys.tail:_*)
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
  def seqIterator(topFirst:Boolean):Iterator[(Seq[K], This)] = new TreeIterator(scala.collection.immutable.Queue.empty,topFirst)
  
  private class TreeIterator(val cur:scala.collection.immutable.Queue[K],topFirst:Boolean) extends Iterator[(Seq[K], This)] {
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
        (cur,MapTreeLikeImmutable.this)
      } else                                                   //if the next is not the current item, then it is the current sub-ioterator next element
        i.next
    }
    private def getSub:Iterator[(Seq[K], This)] = {
      if (iter.hasNext) {                                      //move to next element
        val (k,t)=iter.next
        if (!t.isRef)                                          //if not already processed
          new t.TreeIterator(cur.enqueue(k),topFirst)          //fetch sub-iterator
        else {
          Iterator((cur.enqueue(k),t))                         //iterate superficially on self-references (otherwise you might get an infinite loop)
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
  /** flattens the tree to it's canonical state */
  def flatten:Seq[(Seq[K],This)] = seqIterator(true).toSeq
  /** removes elements */
  override def filter(f:((K,This))=>Boolean):This = {
    val l = for (x <- iterator if f(x)) yield x._1
    for (x <- l) this -= x
    for (x <- iterator) x._2.filter(f)
    this
  }
  
  
  /* TODO
  /** Creates a new Tree T with the same structure, possibly transforming all Keys, Values and even underlying Map representation */
  def map[K1,V1,T<:MapTreeLikeImmutable[K1,V1,T]](f:((K,This))=>(K1,T)):T
  /** 'Replaces' T with f(this):S in all nodes */
  def map[S](f:(This)=>Option[S]):builder.Repr[S] = {
    def doIt(t:This,prev: =>builder.Repr[S]):builder.Repr[S] = {
      var res = null.asInstanceOf[builder.Repr[S]]
      val n = builder.emptyMap[builder.Repr[S]]
      t.self.foreach { u => n.put(u._1,doIt(u._2,res)) }
      res = builder.mapBuild(t.name,f(t),n,prev)
      res
    }
    doIt(this,null.asInstanceOf[builder.Repr[S]])
  }
  /** 'Develops' f(this) in all nodes, enlarging the tree */
  def flatMap[S](f:(This)=>builder.Repr[S]):builder.Repr[S] =
    builder.mapFromCanonical(for (x <- flatten; z <- f(x).canonical) yield (x.parents.map(_.name) ::: z._1, z._2))  
  */

}