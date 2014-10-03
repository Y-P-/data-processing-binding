package utils.tree2

import scala.collection.IterableLike
import scala.collection.generic.Subtractable
import scala.collection.AbstractSet
import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.annotation.tailrec
import scala.runtime.AbstractPartialFunction
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversable
import scala.collection.mutable.ListBuffer

/** Describes a tree where data is reached through a succession of keys.
 *  The actual data of type V is optionnal in intermediary nodes, but a well formed tree should not have
 *  empty leaves. Using the methods with (GenTraversable[K],V) parameters (flat view of the tree) rely
 *  on that property (they will omit to output such degenerate branches, and will not build them.)
 *  
 *  Operations on trees are not to take lightly ; the recursive nature of trees usually involves that
 *  any transformation be done by rebuilding it. This is even true of the withFilter operation : at this
 *  stage, there is yet no `view` for trees.
 */
trait PrefixTreeLike[K, +V, +This <: PrefixTreeLike[K, V, This]]
  extends PartialFunction[K, This]
     with IterableLike[(K, This), This]
     with Subtractable[K, This]
     with Equals { self:This =>
  
  /** The value for the current node */
  def value: Option[V]
  protected[this] type Repr = This
  override def repr:Repr = self

  /** The empty tree of the same type as this tree
   *   @return   an empty tree of type `This`.
   */
  def empty: Repr = newBuilder.empty
    
  /** A common implementation of `newBuilder` for all maps in terms of `empty`.
   *  Overridden for mutable maps in `mutable.MapLike`.
   *  Note that this newBuilder cannot build a mapped Tree (i.e. K, V, Repr different from original.)
   *  This limitation leads us to introduce a new kind of builder, which is more generic.
   */
  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]
  
  /** Removes a key from this tree, returning a new tree.
   *  @param    key the key to be removed
   *  @return   a new tree without a binding for `key`
   *
   *  @usecase  def - (key: A): Map[A, B]
   *    @inheritdoc
   */
  def - (key: K): Repr
  
  /** Adds a key/(value,tree) pair to this tree, returning a new tree.
   *  This standard operation uses the merge choice rather than the replace choice.
   *  Enlarge subtrees without losing information is usualy the expected operation on trees.
   *  @param    kv the key/(value,tree) pair
   *  @tparam   T1 the type of the added tree
   *  @tparam   V1 the type of the added value
   *  @return   a new tree with the new binding added to this tree
   *
   *  @usecase  def + (kv: (K, (V,T)): T
   */
  def + [L>:K,T>:Repr<:PrefixTreeLike[L,_,T]] (kv:(L,T))(implicit replace:Boolean): T = update(kv)

  /** Creates a new iterator over all key/value pairs of this tree.
   *  This iterates only on the immediate children.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[(K, Repr)]

  /** Optionally returns the value associated with a key.
   *
   *  @param  key    the key value
   *  @return an option value containing the value associated with `key` in this tree,
   *          or `None` if none exists.
   */
  def get(key: K): Option[Repr]
  
  /** Retrieves the value which is associated with the given key. This
   *  method invokes the `default` method of the tree if there is no mapping
   *  from the given key to a value. Unless overridden, the `default` method throws a
   *  `NoSuchElementException`.
   *
   *  @param  key the key
   *  @return     the value associated with the given key, or the result of the
   *              tree's `default` method, if none exists.
   */
  def apply(key: K): Repr = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  /**  Returns the value associated with a key, or a default value if the key is not contained in the tree.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the tree.
   *   @tparam  D        the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *
   *   @usecase def getOrElse(key: A, default: => B): B
   *     @inheritdoc
   */
  def getOrElse[T>:Repr](key: K, default: => T): T= get(key).getOrElse(default)


  /** Tests whether this tree contains a binding for a key.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def contains(key: K): Boolean = get(key).isDefined

  /** Tests whether this tree contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def isDefinedAt(key: K): Boolean = contains(key)
  
  def seq: Repr = this

  /** Collects all keys of this tree in a set.
   * @return  a set containing all keys of this tree.
   */
  def keySet: Set[K] = new DefaultKeySet

  /** The implementation class of the set returned by `keySet`.
   */
  protected class DefaultKeySet extends AbstractSet[K] with Set[K] with Serializable {
    def contains(key : K) = self.contains(key)
    def iterator = keysIterator
    def + (elem: K): Set[K] = Set[K]() ++ this + elem
    def - (elem: K): Set[K] = Set[K]() ++ this - elem
    override def size = self.size
    override def foreach[C](f: K => C) = self.keysIterator foreach f
  }

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[K] = new AbstractIterator[K] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._1
  }

  /** The implementation class of the iterable returned by `values`.
   */
  protected class DefaultValuesIterable extends AbstractIterable[Repr] with Iterable[Repr] with Serializable {
    def iterator = valuesIterator
    override def size = self.size
    override def foreach[C](f: Repr => C) = self.valuesIterator foreach f
  }

  /** Creates an iterator for all values in this tree.
   *
   *  @return an iterator over all values that are associated with some key in this tree.
   */
  def valuesIterator: Iterator[Repr] = new AbstractIterator[Repr] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._2
  }

  /** Defines the default value computation for the tree,
   *  returned when a key is not found
   *  The method implemented here throws an exception,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value for which a binding is missing.
   *  @throws `NoSuchElementException`
   */
  def default(key: K): Repr = throw new NoSuchElementException("key not found: " + key)
  
  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original tree without copying any elements.
   */
  def filterKeys(p: K => Boolean): This

  /** Transforms this tree by applying a function to every retrieved value.
   *  @param  f   the function used to transform the values of this tree.
   *  @return a tree which maps every element of this tree.
   *            The resulting tree is a new tree.
   */
  def map[W,T<:PrefixTreeLike[K,W,T]](f:V=>W)(implicit bf:PrefixTreeLikeBuilder[K,W,T], replace:Boolean):T = {
    var b = bf(value.map(f))
    for (x <- this) b += ((x._1,x._2.map(f)))
    b
  }
  
  /** Transforms this tree by applying a function to every retrieved value and key.
   *  @param  f the function used to transform the keys and values of this tree.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree. 
   */
  def mapFull[L,W,T<:PrefixTreeLike[L,W,T]](f:(K=>L,V=>W))(implicit bf:PrefixTreeLikeBuilder[L,W,T], replace:Boolean):T = {
    var b = bf(value.map(f._2))
    for (x <- this) b += ((f._1(x._1),x._2.mapFull(f)))
    b
  }
  
  /** Transforms this tree by applying a function to every key.
   *  @param  f the function used to transform the keys of this tree.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree.
   */
  def mapKeys[L,W>:V,T<:PrefixTreeLike[L,W,T]](f:K=>L)(implicit bf:PrefixTreeLikeBuilder[L,W,T], replace:Boolean):T = {
    var b = bf(value)
    for (x <- this) b += ((f(x._1),x._2.mapKeys[L,W,T](f)))
    b
  }
    
  /** Transforms this tree by applying a function to every retrieved value.
   *  @param  f   the function used to transform the values of this tree.
   *  @return a tree which maps every value of this tree to a new tree with same key type
   *            The resulting tree is a new tree with the same key type, the new value type,
   *            which expands this tree values to new subtrees.
   */
  def flatMap[W,T<:PrefixTreeLike[K,W,T]](f:V=>T)(implicit bf:PrefixTreeLikeBuilder[K,W,T], replace:Boolean):T = {
    if (value.isDefined) {
      val r = f(value.get)
      var b = bf(r.value)
      for (x <- r) b += x
      for (x <- this) b += ((x._1,x._2.flatMap[W,T](f)))
      b
    } else {
      var b = bf.empty
      for (x <- this) b += ((x._1,x._2.flatMap[W,T](f)))
      b
    }
  }
  
  /** Creates a new tree obtained by updating this tree with a given key/value pair.
   *  @param    kv the key/value pair
   *  @param    replace if true, the existing value is discarded, if false it is merged
   *  @tparam   L the type of the new keys
   *  @tparam   T the type of the added value
   *  @return   A new tree with the new key/value mapping added to this map.
   */
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](kv:(L,T))(implicit replace:Boolean): T
  
  /** Identical to the previous method, but more than one element is updated.
   */
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](replace:Boolean,kv:(L,T)*): T = update(replace,kv:_*)
  
  /** Identical to the previous method, but elements are passed through an iterable like rather than a built Seq.
   */
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](replace:Boolean,kv:GenTraversableOnce[(L,T)]): T =
    kv.foldLeft[T](repr)(_.update(_)(replace))

  /** Adds key/value pairs to this tree, returning a new tree.
   *
   *  This method takes two or more key/value pairs. Another overloaded
   *  variant of this method handles the case where a single key/value pair is
   *  added.
   *  @param    kv1 the first key/value pair
   *  @param    kv2 the second key/value pair
   *  @param    kvs the remaining key/value pairs
   *  @tparam   V1  the type of the added values
   *  @tparam   T1  the type of the added tree
   *  @return   a new tree with the given bindings added to this tree
   *
   *  @usecase  def + (kvs: (A, B)*): Map[A, B]
   *    @inheritdoc
   *    @param    kvs the key/value pairs
   */
  def + [L>:K,T>:Repr<:PrefixTreeLike[L,_,T]] (kv1:(L,T), kv2:(L,T), kvs:(L,T) *)(implicit replace:Boolean): T =
    this + kv1 + kv2 ++ kvs

  /** Adds all key/value pairs in a traversable collection to this tree, returning a new map.
   *
   *  @param    xs  the collection containing the added key/value pairs
   *  @tparam   B1  the type of the added values
   *  @return   a new tree with the given bindings added to this tree
   *
   *  @usecase  def ++ (xs: Traversable[(A, B)]): Map[A, B]
   *    @inheritdoc
   */
  def ++[T1 >: Repr <: PrefixTreeLike[K,_,T1]](xs: GenTraversableOnce[(K, T1)])(implicit replace:Boolean): T1 =
    ((repr: T1) /: xs.seq) (_ + _)

  /** Returns a new tree obtained by removing all key/value pairs for which the predicate
   *  `p` returns `true`.
   *
   *  '''Note:'''    This method works by successively removing elements for which the
   *           predicate is true from this set.
   *           If removal is slow, or you expect that most elements of the set
   *           will be removed, you might consider using `filter`
   *           with a negated predicate instead.
   *  @param p    A predicate over key-value pairs
   *  @return     A new tree containing elements not satisfying the predicate.
   */
  override def filterNot(p: ((K, Repr)) => Boolean): Repr = {
    var res: Repr = repr
    for (kv <- this)
      if (p(kv)) res -= kv._1
    res
  }

  /* Overridden for efficiency. */
  override def toSeq: Seq[(K, Repr)] = toBuffer[(K, Repr)]
  override def toBuffer[C >: (K, Repr)]: scala.collection.mutable.Buffer[C] = {
    val result = new scala.collection.mutable.ArrayBuffer[C](size)
    copyToBuffer[C](result)
    result
  }
  
  /** Creates the canonical flat representation of the tree.
   *  Working with this supposes that your tree doesn't use degenerate branches (with no children and no value,
   *  i.e. with empty as a node somewhere in the tree)
   *  @return the canonical view of the tree
   */
  def seqView(topFirst:Boolean=true,trackDuplicate:Boolean=false) = new SeqView(topFirst,trackDuplicate)
  
  /** This class is used to iterate deeply through the tree.
   *  @param cur the current sequence of key for the current element (from bottom to top!)
   *  @param topFirst is true if an element appears before its children, false otherwise
   *  @param seen is a set that is updated to track internal cross references ; if null, no such tracking happens (performance gain, but infinite loops possible)
   */
  protected class TreeIterator(cur:Seq[K],topFirst:Boolean,seen:scala.collection.mutable.Set[Repr]) extends AbstractIterator[(Seq[K], This)] {
    if (seen!=null) seen.add(repr)                             //remember that this tree is already being processed, to avoid loops in trees containing self-references 
    protected[this] val iter = iterator                        //iterator for this level
    protected[this] var i:Iterator[(Seq[K], This)] = getSub    //current sub-iterator
    protected[this] var done:Boolean = false                   //true when this item has been provided
    @tailrec final def hasNext():Boolean = !done || i!=null && (i.hasNext || {i=getSub; hasNext})
      //some clarifications: if this item has not been processed, there is a next
      //if there is no sub-iterator available and this item has been processed, we are finished
      //but if there is a sub-iterator with a next element, then there is a next
      //otherwise fetch the next sub-iterator (which could be null) and then check if it has a next element
    final def next(): (Seq[K], This) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,repr)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    final def getSub:Iterator[(Seq[K], This)] = {
      if (iter.hasNext) {                                      //move to next element
        val (k,t)=iter.next
        if (seen==null || !seen.contains(t))                   //if not already processed
          new t.TreeIterator(cur.+:(k),topFirst,seen)          //fetch sub-iterator
        else
          Iterator((cur.+:(k),t))                              //iterate superficially on self-references (otherwise you might get an infinite loop)
      } else
        null                                                   //return null when finished
    }
  }
  
  /** The Tree can conveniently be viewed almost as a 'Map' with sequences of keys as key.
   *  This is very convenient to iterate through it.
   *  This also provides the best way to transform a Tree: it is much easier to transform the canonical form into
   *  a new canonical form and build a new tree from that.
   */
  protected class SeqView(topFirst:Boolean,trackDuplicate:Boolean) extends PartialFunction[GenTraversableOnce[K], This] with Iterable[(Seq[K], V)] {
    def iterator:Iterator[(Seq[K], V)] = new Iterator[(Seq[K], V)] {
      val i = new TreeIterator(Nil,topFirst,if (trackDuplicate) scala.collection.mutable.Set.empty else null)
      @tailrec def fetch:(Seq[K], This) = { if (!i.hasNext) null else { var x=i.next(); if (x._2.value.isDefined) x else fetch } }
      var cur = fetch
      def hasNext: Boolean = cur!=null
      def next(): (Seq[K], V) = { val c=cur; cur=fetch; (c._1.reverse,c._2.value.get) }
    }
    final def tree                                       = self
    def apply(keys:GenTraversableOnce[K]):Repr           = keys.foldLeft(self)(_(_))
    def isDefinedAt(keys: GenTraversableOnce[K]):Boolean = get(keys)!=None
    def get(keys:GenTraversableOnce[K]):Option[Repr]     = keys.foldLeft[Option[This]](Some(self))((t,k)=>if (t==None) None else t.get.get(k))
    def apply(keys:K*):Repr                              = apply(keys)
    def get(keys:K*):Option[Repr]                        = get(keys)
    /** Transforms this seqView by applying a function to every retrieved value and key.
     *  @param  f the function used to transform the keys and values of this tree.
     *  @return a tree which maps every element of this tree.
     *          The resulting tree is a new tree. 
     */
    def flatMap[W](f:V=>GenTraversable[(GenTraversable[K],W)]):GenTraversable[(GenTraversable[K],W)] =
      for (x <- iterator.toTraversable; r <- f(x._2)) yield ((x._1 ++ r._1, r._2))
  }
  
  /** Appends all bindings of this tree to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string `end`.
   *  Inside, the string representations of all bindings of this tree.
   *  in the form of `key -> value` are separated by the string `sep`.
   *
   *  @param b     the builder to which strings are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    this.iterator.map { case (k, v) => k+" -> "+v }.addString(b, start, sep, end)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every tree is `"Map"`.
   */
  override def stringPrefix: String = value match {
    case None    => "Tree"
    case Some(v) => "Tree{"+v+"}"
  }

  override def toString = super[IterableLike].toString
}

object PrefixTreeLike {
  implicit def toSeq[T<:PrefixTreeLike[_,_,T]](t:T):t.SeqView = t.seqView()
}

/** An abstract class for the trait. Used to share code.
 */
abstract class AbstractPrefixTreeLike[K, +V, +This <: AbstractPrefixTreeLike[K, V, This]] extends AbstractPartialFunction[K, This] with PrefixTreeLike[K, V, This] {
  this:This=>
  override def apply(key: K): Repr = super[PrefixTreeLike].apply(key)
}

/*
  def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    foreach(pf.runWith(b += _))
    b.result
  }

  /** Builds a new collection by applying an option-valued function to all
   *  elements of this $coll on which the function is defined.
   *
   *  @param f      the option-valued function which filters and maps the $coll.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the option-valued function
   *                `f` to each element and collecting all defined results.
   *                The order of the elements is preserved.
   *
   *  @usecase def filterMap[B](f: A => Option[B]): $Coll[B]
   *    @inheritdoc
   *
   *    @param pf     the partial function which filters and maps the $coll.
   *    @return       a new $coll resulting from applying the given option-valued function
   *                  `f` to each element and collecting all defined results.
   *                  The order of the elements is preserved.
  def filterMap[B, That](f: A => Option[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this)
      f(x) match {
        case Some(y) => b += y
        case _ =>
      }
    b.result
  }
   */

  /** Partitions this $coll in two ${coll}s according to a predicate.
   *
   *  @param p the predicate on which to partition.
   *  @return  a pair of ${coll}s: the first $coll consists of all elements that
   *           satisfy the predicate `p` and the second $coll consists of all elements
   *           that don't. The relative order of the elements in the resulting ${coll}s
   *           is the same as in the original $coll.
   */
  def partition(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    for (x <- this) (if (p(x)) l else r) += x
    (l.result, r.result)
  }
 */
