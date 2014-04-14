package utils.tree

import scala.annotation.tailrec

/** A Tree is an association of key/values.
 *  Unlike Maps, this association goes "in depth".
 *  For example:
 *     t: TreeBase[String,Int]
 *     val t1 = t -> "a" -> "b" -> "c"   //fetches subtree for the composite key a.b.c
 *     val v1 = t1.value                 //fetches the values for the composite key a.b.c
 *     val t2 = t1 -> "d"                //fetches subtree for the composite key a.b.c.d
 *     val v2 = t2.value                 //fetches the values for the composite key a.b.c.d
 *  Although the description could lead to believe that the implementation relies on maps,
 *  there actually is no such supposition, even though an implementation based on maps is
 *  actually natural!
 *  Note in particular that the abstract methods that are presented are the same as in MapLike.
 *  There is no coincidence : we want to be able to back Trees on existing maps implementations.
 */
trait TreeBase[K,+V,+This<:TreeBase[K,V,This]] {self=>
  /** value for this key */
  def value:Option[V]
  
  //All of the following methods have equivalence in Map
  /** subtree for this key */
  def get(key:K):Option[This]
  /** default value */
  def default(key:K):This
  /** iterator */
  def iterator: Iterator[(K, This)]
  /** add element */
  def + [T >: This](kv: (K, T)): This
  /** remove element */
  def -(key: K): This
  /** empty implementation */
  def empty: This
  /** tells if the subtree is empty */
  def isEmpty:Boolean
  
  //local implementations
  /** nice way to reach an item. e.g tree->"a"->"x" looks better than t("a")("x") */
  final def ->(key:K):This = apply(key)
  /** subtree for that key using defaults if necessary : same implementation as Map. */
  def apply(key:K):This = get(key) match {
    case None    => default(key)
    case Some(x) => x
  }
  
  //methods on sequences of keys
  /** Adds or replaces T for the given sequence of keys */
  def add [T >: This](keys:Seq[K], tree:T): This = {
    val x = keys(0)
    this + (x, (if (keys.length==1) tree else (get(x) match {
      case None    => empty
      case Some(m) => m 
    }) add (keys.tail, tree)))
  }
  /** Removes the value for the given sequence of keys.
   *  If the result is empty and has no value, the truncated key is removed too.
   *  This happens as long as the conditions holds walking up the sequence.
   *  Ex: this rem ("a","b","c") first removes "a"->"b"->"c"
   *      then it checks if "a"->"b" is still significant (has a proper value or non empty subtree)
   *      if not it removes it, and in that case it proceeds to "a"...
   */
  def rem (keys:K*): This = {
    val x = keys(0)
    val m = get(x)
    if (keys.length==1 || m.isEmpty)    this - x
    else {
      val r = m.get rem (keys.tail:_*)
      if (r.isEmpty && r.value.isEmpty) this rem (keys.init:_*)
      else                              this + (x,r)
    }
  }
  /** subtree for this sequence of keys, without using any default */
  def get(keys:K*):Option[This] = {
    val k = keys(0)
    val r = get(k)
    if (keys.length==1) r else (r match {
      case None    => return None
      case Some(x) => x
    }).get(keys.tail:_*)
  }
  /** subtree for this sequence of keys, using all possible defaults */
  def apply(keys:K*):This = {
    val k = keys(0)
    val r = (get(k) match {
      case None    => default(k)
      case Some(x) => x
    })
    if (keys.length==1) r else r(keys.tail:_*)
  }
  def seqIterator:Iterator[(Seq[K], This)] = this
}

object TreeBase {
  /** The Tree can conveniently be viewed as a Map of sequences of keys.
   */
  implicit class SeqTree[K,+V,+This<:TreeBase[K,V,This]](val self:TreeBase[K,V,This]) extends Map[Seq[K],This] {
    def get(keys:Seq[K]):Option[This]                    = self.get(keys:_*)
    def + [T >: This](kv: (Seq[K], T)): Map[Seq[K],This] = self.add(kv._1,kv._2)
    def - (keys: Seq[K]): Map[Seq[K],This]               = self.rem(keys:_*)
    def iterator: Iterator[(Seq[K], This)]               = self
    override def apply(keys: Seq[K]):This                = self.apply(keys:_*)
  }
  
  implicit class TreeIterator[K,+V,+This<:TreeBase[K,V,This]](tree:TreeBase[K,V,This]) extends TreeIteratorBase(tree,scala.collection.immutable.Queue.empty,Iterator.empty)
  
  class TreeIteratorBase[K,+V,+This<:TreeBase[K,V,This]](tree:TreeBase[K,V,This],val cur:scala.collection.immutable.Queue[K],parent:Iterator[(Seq[K], This)]) extends Iterator[(Seq[K], This)] {
    protected[this] var i:Iterator[(Seq[K], This)] = Iterator.empty     //current sub-iterator
    val iter = tree.iterator                                            //iterator for this level
    def hasNext:Boolean = i.hasNext || parent.hasNext                   //there is a next iif there is one at this level or at the parent-level
    def next(): (Seq[K], This) = {
      if (i.isEmpty) {
        val (k,t)=iter.next
        i=new TreeIteratorBase(t,cur.enqueue(k),this)
      }
      i.next
    }
  }
}

/*
trait TreeMapLike[K,+V,+This<:TreeMapLike[K,V,This]] extends Map[K,This] with TreeBase[K,V,This] {
  override def apply(key:K):This = super[TreeBase].apply(key)
}

trait TreeMap[K,+V] extends TreeMapLike[K,V,TreeMap[K,V]]

object U {
  val t = new TreeMap[String,Integer] {
    
  }
  val x = (t->"a"->"b"->"c").value.get
}*/