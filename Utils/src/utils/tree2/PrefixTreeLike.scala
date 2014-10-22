package utils.tree2

import scala.collection.IterableLike
import scala.collection.AbstractSet
import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.Subtractable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.runtime.AbstractPartialFunction
import scala.collection.mutable.Buffer
import scala.collection.TraversableOnce
import scala.util.Try

/** Describes a tree where data is reached through a succession of keys.
 *  The actual data of type V is optionnal in intermediary nodes, but a well formed tree should not have
 *  empty leaves. Using the methods with (GenTraversable[K],V) parameters (flat view of the tree) rely
 *  on that property (they will omit to output such degenerate branches, and will not build them.)
 *  
 *  Operations on trees are not to take lightly ; the recursive nature of trees usually involves that
 *  any transformation be done by rebuilding it. This is even true of the withFilter operation : at this
 *  stage, there is yet no `view` for trees.
 *  
 *  The `default` operation usually suggests that it can handle any other value not listed in the
 *  iterable part of the tree
 *  It must be differentiated from the `orElse` operation which links on a fallback tree.
 */
trait PrefixTreeLike[K, +V, +This <: PrefixTreeLike[K, V, This]]
  extends PartialFunction[K, This]
     with IterableLike[(K, This), This]
     with Subtractable[K, This]
     with Equals { self:This =>
  //an alias that concrete classes can use as shrotcut to refer to themselves ; necessary too to get the appropriate overload on apply
  protected[this] type Repr = This
  override def repr:Repr = self
  
  /** The value for the current node */
  def value: Option[V]
  
  /** Defines the default value computation for the tree, returned when a key is not found.
   *  It can be null, in which case a fallback default (usually an exception) is used.
   *  This is assumed to throw `NoSuchElementException` when it doesn't handle a given key.
   *
   *  @param key the given key value for which a binding is missing.
   */
  def default: K=>Repr = null
  
  /** Used in place of default when the latter is null.
   *  This is not a significant method (it throws an exception.)
   */
  def genericDefault: K=>Nothing
      
  /** The empty tree of the same type as this tree
   *  @return   an empty tree of type `This`.
   */
  def empty: Repr
    
  /** A new instance of builder similar to the one used to build this tree element.
   *  It can be used to build elements of the same tree kind.
   */
  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]
  
  /** rebuilds this tree with a specific default */
  final def withDefault[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](default:K=>T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = bf.withDefault(this, default)
  /** rebuilds this tree with a specific value */
  final def withValue[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](value:Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = bf.withValue(this, value)
  
  /** Removes a key from this tree, returning a new tree.
   *  @param    key the key to be removed
   *  @return   a new tree without a binding for `key`
   *
   *  @usecase  def - (key: A): Map[A, B]
   *    @inheritdoc
   */
  def - (key: K): Repr
  
  /** Adds a key/(value,tree) pair to this tree, returning a new tree.
   *  Enlarge subtrees without losing information is usualy the expected operation on trees.
   *  @param    kv the key/(value,tree) pair
   *  @tparam   T1 the type of the added tree
   *  @tparam   V1 the type of the added value
   *  @return   a new tree with the new binding added to this tree
   *
   *  @usecase  def + (kv: (K, (V,T)): T
   */
  def + [W>:V,T>:Repr<:PrefixTreeLike[K,W,T]] (kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = update[W,T](kv)

  /** Creates a new iterator over all key/value pairs of this tree.
   *  This iterates only on the immediate children.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[(K, Repr)]

  /** Optionally returns the value associated with a key.
   *  The basic supposition is that keys found in the iterator yield a value, others yield a None.
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
    case None => if (default!=null) default(key) else genericDefault(key)
    case Some(value) => value
  }
  
  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  def isNonSignificant = false 

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable tree consisting only of those key where the key satisfies
   *          the predicate `p`. This results in a new tree.
   */
  def filterKeys(p: K => Boolean): Repr  = newBuilder(value,iterator.filter(x=>p(x._1)),default)
  
  /** Filters this map by retaining only key/value satisfying a predicate.
   *  @param  p   the predicate used to test key/value
   *  @return an immutable map consisting only of those key value pairs of this map where the key/value satisfies
   *          the predicate `p`. This results in a new tree.
   */
  def filterAll(p: ((K,Repr)) => Boolean): Repr  = {
    def h(r:Repr):Repr = newBuilder(r.value,r.filter(p).map((x:(K,Repr))=>(x._1,h(x._2))),r.default)
    h(this)
  }

  /**  Returns the value associated with a key, or a default value if the key is not contained in the tree.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the tree.
   *   @tparam  D        the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *
   *   @usecase def getOrElse(key: A, default: A => B): B
   *     @inheritdoc
   */
  def getOrElse[T>:Repr](key: K, default: => T): T = get(key).getOrElse(default)


  /** Tests whether this tree contains a binding for a key.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def contains(key: K): Boolean = get(key).isDefined

  /** Tests whether this tree contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`. Note that the default function doesn't 
   *  qualify for the success of `isDefinedAt` in the standard implementation.
   *  However it is possible to overload this method to account for the values
   *  covered by the default.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def isDefinedAt(key: K): Boolean = contains(key)
  
  def seq: Repr = this

  /** The implementation class of the set returned by `keySet`.
   */
  protected class DefaultKeySet extends AbstractSet[K] with Set[K] with Serializable {
    def contains(key : K) = self.contains(key)
    def iterator = keysIterator
    def + (elem: K): Set[K] = Set.empty[K] ++ this + elem
    def - (elem: K): Set[K] = Set.empty[K] ++ this - elem
    override def size = self.size
    override def foreach[C](f: K => C) = self.keysIterator foreach f
  }

  /** Creates an iterator for all keys.
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[K] = new AbstractIterator[K] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._1
  }
  /** Creates an iterator for all values in this tree.
   *  @return an iterator over all values that are associated with some key in this tree.
   */
  def valuesIterator: Iterator[Repr] = new AbstractIterator[Repr] {
    val iter = iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._2
  }

  /** The implementation class of the iterable returned by `values`.
   */
  protected class DefaultValuesIterable extends AbstractIterable[Repr] with Iterable[Repr] with Serializable {
    def iterator = valuesIterator
    override def size = self.size
    override def foreach[C](f: Repr => C) = iterator.foreach(f)
  }
  
  /** Collects all keys of this tree in a set.
   * @return  a set containing all keys of this tree.
   */
  def keySet: Set[K] = new DefaultKeySet
  
  /** Returns an iterable over the defined values, these for which isDefinedAt is true.
   */
  def values:Iterable[Repr] = new DefaultValuesIterable

  /** Creates a new tree by combining this tree with a fallback tree.
   *  This builds a new tree, which keeps the value and default of this tree.
   */
  def orElse[W>:V, T>:Repr<:PrefixTreeLike[K,W,T]](that: T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    bf(value,that ++ this,default)
  
  /** An internal class designed to make efficient zip operations without code duplication between various zip cases. 
   */
  protected[this] class Zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](implicit bf:PrefixTreeLikeBuilder[K,U,R]) {
    trait Recur[+RR<:Recur[RR]] { this:RR=>
      def value(t:T,cur:Repr):Option[U]
      def next(k:K):RR
      def default(t:T,cur:Repr):K=>R  = if (cur.default==null) null else k=>recur(t(k),cur.default(k),next(k))
      def loop(cur:Repr,t:T,r:RR)     = for (x:(K,This) <- cur) yield (x._1, recur(t(x._1),x._2,next(x._1)))
      def recur(t:T,cur:Repr,r:RR):R  = bf(value(t,cur),loop(cur,t,r),default(t,cur))
      def apply(t:T,cur:Repr):R       = recur(t,cur,this)
    }
    //zip tolerating non matching trees (the intersection only will match)
    trait RecurPartial[+RR<:RecurPartial[RR]] extends Recur[RR] { this:RR=>
      override def loop(cur:Repr,t:T,r:RR)    = for (x:(K,This) <- cur; v <- try { List((t(x._1),next(x._1))) } catch { case _:NoSuchElementException => Nil }) yield (x._1, recur(v._1,x._2,v._2))      
      override def default(t:T,cur:Repr):K=>R = if (cur.default==null) null else k=> { val d=cur.default(k); try { recur(t(k),d,next(k)) } catch { case _:NoSuchElementException => bf.empty } }
    }
    //zip for tree operations (the operation between T and This depends on the current node)
    class RecurOpTree[O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O]](protected[this] val op:O) extends Recur[RecurOpTree[O]] {
      def value(t:T,cur:Repr):Option[U] = if (op.value.isEmpty) None else op.value.get(t,cur)
      def next(k:K):RecurOpTree[O]      = new RecurOpTree(op(k))
    }
    class RecurOpTreePartial[O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O]](op:O) extends RecurOpTree[O](op) with RecurPartial[RecurOpTreePartial[O]] {
      override def next(k:K):RecurOpTreePartial[O] = new RecurOpTreePartial(op(k))      
    }
    //zip for constant operations
    class RecurOp(op:(T,Repr)=>Option[U]) extends Recur[RecurOp] {
      def value(t:T,cur:Repr):Option[U] = op(t,cur)
      def next(k:K):this.type           = this
    }
    class RecurOpPartial(op:(T,Repr)=>Option[U]) extends RecurOp(op) with RecurPartial[RecurOpPartial]
  }
  
  /** Transform this tree with another tree.
   *  Both trees are explored 'in parallel' and each sub-node of this tree is transformed using the
   *  corresponding sub-node of the transformer tree using the provided `op`.
   *  Default values for the second tree are used but exceptions will end the transformation ungracefully.
   *  The resulting tree contains no default.
   *  @param t   the transformer tree
   *  @param op  an operator that transforms a T node with a This node giving the expected U result
   *  @return the resulting transformed tree
   */
  def zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,op:(T,Repr)=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    val z = new Zip[U,T,R]
    new z.RecurOp(op)(t,this)
    //def recur(tt:T,cur:Repr):R = bf(op(tt,cur),for (x:(K,This) <- cur) yield (x._1, recur(tt(x._1),x._2)))
    //recur(t,this)
  }
  
  /** Similar to the previous method, but the operation is provided through a third tree which is explored
   *  'in parallel' too.
   *  Default values for both trees are used but exceptions will end the transformation ungracefully.
   *  The resulting tree contains no default.
   *  Using a constant transformer tree (for op) achieves the same result as `zip` above, but is more costly.
   *  @param t   the transformer tree
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node
   *  @return the resulting transformed tree
   */
  def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](op:O)(t:T)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    val z = new Zip[U,T,R]
    new z.RecurOpTree[O](op)(t,this)
    //def recur(tt:T,cur:Repr,opp:O):R = bf(opp.value.flatMap(_(tt,cur)),for (x:(K,This) <- cur) yield (x._1, recur(tt(x._1),x._2,opp(x._1))))
    //recur(t,this,op)
  }
  
  /** Similar to `zip` above.
   *  However, errors in fetching the T tree elements that result in NoSuchElementException are simply removed from
   *  the final result and do not stop the computation. Other exceptions still apply normally.
   *  Note that using this method is more costly than the above and the previous 'zip' should be preferred whenever possible.
   */
  def zipPartial[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](op:(T,Repr)=>Option[U])(t:T)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    val z = new Zip[U,T,R]
    new z.RecurOpPartial(op)(t,this)
    //def recur(tt:T,cur:Repr):R = bf(op(tt,cur),for (x:(K,This) <- cur; v <- try { Buffer(tt(x._1)) } catch { case _:NoSuchElementException => Nil }) yield (x._1, recur(v,x._2)))
    //recur(t,this)
  }
  
  /** Similar to `zip` above.
   *  However, errors in fetching the T or O trees elements that result in NoSuchElementException are simply removed from
   *  the final result and do not stop the computation. Other exceptions still apply normally.
   *  Note that using this method is more costly than the above and the previous 'zip' should be preferred whenever possible.
   */
  def zip2Partial[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](op:O)(t:T)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    val z = new Zip[U,T,R]
    new z.RecurOpTreePartial[O](op)(t,this)
    //def recur(tt:T,cur:Repr,opp:O):R = bf(opp.value.flatMap(_(tt,cur)),for (x:(K,This) <- cur; v <- try { List((tt(x._1),opp(x._1))) } catch { case _:NoSuchElementException => Nil }) yield (x._1, recur(v._1,x._2,v._2)))
    //recur(t,this,op)
  }

  /** Transforms this tree by applying a function to every retrieved value.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f   the function used to transform the values of this tree.
   *  @return a tree which maps every element of this tree.
   *            The resulting tree is a new tree.
   */
  def map[W,T<:PrefixTreeLike[K,W,T]](f:V=>W)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = {
    var b = bf(value.map(f),if (default==null) null else default(_:K).map(f))
    for (x <- this) b += ((x._1,x._2.map(f)))
    b
  }
  
  /** Transforms this tree by applying a function to every retrieved value and key.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f the function used to transform the keys and values of this tree.
   *            it must be bijective in K<=>L if there are any default involved,
   *            and in that case, f._2 must be given ; otherwise it can be null
   *            and bijectivity is not compulsory
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree. 
   */
  def mapFull[L,W,T<:PrefixTreeLike[L,W,T]](f:(K=>L,L=>K,V=>W))(implicit bf:PrefixTreeLikeBuilder[L,W,T]):T =
    bf(value.map(f._3), for (x:(K,This) <- this) yield ((f._1(x._1),x._2.mapFull(f))), if (default==null) null else (l:L)=>default(f._2(l)).mapFull(f))
  
  /** Transforms this tree by applying a function to every key.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f the function used to transform the keys of this tree.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree.
   */
  def mapKeys[L,W>:V,T<:PrefixTreeLike[L,W,T]](f:(K=>L,L=>K))(implicit bf:PrefixTreeLikeBuilder[L,W,T]):T =
    bf(value, for (x:(K,This) <- this) yield ((f._1(x._1),x._2.mapKeys[L,W,T](f))), if (default==null) null else (l:L)=>default(f._2(l)).mapKeys[L,W,T](f))
    
  /** Transforms this tree by applying a function to every retrieved value.
   *  It works also on default values.
   *  This is a non intuitive transformation that should be handled with care.
   *  In case of conflict on expansion, the old value is preserved.
   *  Note: this is best used to expand trees with only leaves with values: in that case
   *        the transformation makes sense:
   *        - intermediate nodes are unaffected (no value to expand)
   *        - terminal nodes are expanded downward with the trees attached to the held value
   *  @param  f  the function used to transform the values of this tree.
   *  @return a tree which maps every value of this tree to a new tree with same key type
   *            The resulting tree is a new tree with the same key type, the new value type,
   *            which expands this tree values to new subtrees.
   */
  def flatMap[W,T<:PrefixTreeLike[K,W,T]](f:V=>T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = {
    val e = (if (value.isDefined) f(value.get) else bf.empty)
    bf(e.value, e++(for (x:(K,This) <- this) yield ((x._1,x._2.flatMap[W,T](f)))), if (default==null) null else default(_:K).flatMap[W,T](f))
  }
  
  /** Creates a new tree obtained by updating this tree with a given key/value pair.
   *  @param    kv the key/value pair
   *  @tparam   L the type of the new keys
   *  @tparam   T the type of the added value
   *  @return   A new tree with the new key/value mapping added to this map.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T
  
  /** Identical to the previous method, but more than one element is updated.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T)*)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = update[W,T](kv:_*)
  
  /** Identical to the previous method, but elements are passed through an iterable like rather than a built Seq.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    kv.foldLeft[T](repr)(_.update[W,T](_))

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
  def + [W>:V,T>:Repr<:PrefixTreeLike[K,W,T]] (kv1:(K,T), kv2:(K,T), kvs:(K,T) *)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    this +[W,T] kv1 + kv2 ++ kvs

  /** Adds all key/value pairs in a traversable collection to this tree, returning a new tree.
   *
   *  @param    xs  the collection containing the added key/value pairs
   *  @tparam   B1  the type of the added values
   *  @return   a new tree with the given bindings added to this tree
   *
   *  @usecase  def ++ (xs: Traversable[(A, B)]): Map[A, B]
   *    @inheritdoc
   */
  def ++[W>:V, T1 >: Repr <: PrefixTreeLike[K,W,T1]](xs: GenTraversableOnce[(K, T1)])(implicit bf:PrefixTreeLikeBuilder[K,W,T1]): T1 =
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
  override def toBuffer[C >: (K, Repr)]: ArrayBuffer[C] = {
    val result = new ArrayBuffer[C](size)
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
  protected class TreeIterator(cur:Seq[K],topFirst:Boolean,seen:scala.collection.mutable.Set[Repr]) extends AbstractIterator[(Seq[K], Repr)] {
    if (seen!=null) seen.add(repr)                             //remember that this tree is already being processed, to avoid loops in trees containing self-references 
    protected[this] val iter = iterator                        //iterator for this level
    protected[this] var i:Iterator[(Seq[K], Repr)] = getSub    //current sub-iterator
    protected[this] var done:Boolean = false                   //true when this item has been provided
    @tailrec final def hasNext():Boolean = !done || i!=null && (i.hasNext || {i=getSub; hasNext})
      //some clarifications: if this item has not been processed, there is a next
      //if there is no sub-iterator available and this item has been processed, we are finished
      //but if there is a sub-iterator with a next element, then there is a next
      //otherwise fetch the next sub-iterator (which could be null) and then check if it has a next element
    final def next(): (Seq[K], Repr) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,repr)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    final def getSub:Iterator[(Seq[K], Repr)] = {
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
  protected class SeqView(topFirst:Boolean,trackDuplicate:Boolean) extends PartialFunction[GenTraversableOnce[K], Repr] with Iterable[(Seq[K], V)] {
    def iterator:Iterator[(Seq[K], V)] = new Iterator[(Seq[K], V)] {
      val i = new TreeIterator(Nil,topFirst,if (trackDuplicate) scala.collection.mutable.Set.empty else null)
      @tailrec def fetch:(Seq[K], Repr) = { if (!i.hasNext) null else { var x=i.next(); if (x._2.value.isDefined) x else fetch } }
      var cur = fetch
      def hasNext: Boolean = cur!=null
      def next(): (Seq[K], V) = { val c=cur; cur=fetch; (c._1.reverse,c._2.value.get) }
    }
    final def tree                                       = self
    def apply(keys:GenTraversableOnce[K]):Repr           = keys.foldLeft(self)(_(_))
    def isDefinedAt(keys: GenTraversableOnce[K]):Boolean = get(keys)!=None
    def get(keys:GenTraversableOnce[K]):Option[Repr]     = keys.foldLeft[Option[Repr]](Some(self))((t,k)=>if (t==None) None else t.get.get(k))
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
  /** An abstract class for the trait. Used to share code.
   */
  abstract class Abstract[K, +V, +This <: Abstract[K, V, This]] extends AbstractPartialFunction[K, This] with PrefixTreeLike[K, V, This] {
    this:This=>
    override def apply(key: K): Repr = super[PrefixTreeLike].apply(key)
  }
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
