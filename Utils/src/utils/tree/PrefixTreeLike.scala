package utils.tree

import scala.collection.IterableLike
import scala.collection.AbstractSet
import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.Subtractable
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.runtime.AbstractPartialFunction
import java.util.NoSuchElementException

/** Describes a tree where data is reached through a succession of keys.
 *  The actual data of type V is optional in intermediary nodes, but a well formed tree should not have
 *  empty leaves. Using the methods with (GenTraversable[K],V) parameters (flat view of the tree) rely
 *  on that property (they will omit to output such degenerate branches, and will not build them.)
 *  
 *  Operations on trees are not to take lightly ; the recursive nature of trees usually involves that
 *  any transformation be done by rebuilding it. This is even true of the withFilter operation : at this
 *  stage, there is yet no `view` for trees.
 *  
 *  The `default` operation usually suggests that it can handle any other value not listed in the
 *  iterable part of the tree. It itself defaults on a generic (but user provided) method.
 *  
 *  There are usually three kinds of trees, in ascending order of complexity, memory footprint, building speed:
 *  - Standard trees provide no way to ascend into the tree.
 *  - Weak Navigable trees allow access to the parent, BUT it relies on mutation and may sometimes be
 *    unsafe to use ; in particular, a tree that is built using shared branches may have incorrect parents.
 *    These trees can contain sub-parts that are not navigable.
 *  - Strong Navigable trees are just like weak navigable trees, but nodes are always rebuilt on assignment.
 *    In particular, these tree nodes always correctly point to the right parent. However, the building
 *    cost is usually expensive. Nodes are still mutable on the remove operation where the parent is reset to null.
 *    
 *    @tparam K the key type
 *    @tparam V the contained value type
 *    @tparam This the prefix tree type
 */
trait PrefixTreeLike[K, +V, +This <: PrefixTreeLike[K, V, This]]
  extends PartialFunction[K, This]
     with PrefixTraversableOnce[K, V, This]
     with IterableLike[(K, This), This]
     with Subtractable[K, This]
     with Equals { self:This =>
      
  /** The general parameters used for building the tree.
   */
  type Params <: PrefixTreeLike.Params[K,V,Repr]
  def params:Params
  
  /** The value for the current node */
  def value: Option[V]
  
  /** Defines the default value computation for the tree, returned when a key is not found.
   *  It can be null, in which case a fallback default (usually an exception) is used.
   *  This is assumed to throw `NoSuchElementException` when it doesn't handle a given key.
   *
   *  @param key the given key value for which a binding is missing.
   */
  def default: K=>Repr = null
  
  //an internal utility to correctly rebuild default when necessary
  @inline final protected def asDefault[L,T](f:L=>T):L=>T = if (default==null) null else f
  
  /** The empty tree of the same type as this tree
   *  @return   an empty tree of type `This`.
   */
  def empty: Repr = newBuilder(None,Nil,null)
  
  /** A new instance of builder similar to the one used to build this tree element.
   *  It can be used to build elements of the same tree kind.
   */
  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]
  
  /** rebuilds this tree with a specific default */
  final def withDefault[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](default:K=>T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = bf.withDefault(this, default)
  /** rebuilds this tree with a specific value */
  final def withValue[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](value:Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = bf.withValue(this, value)
  /** rebuilds this tree as defaulting to itself */
  final def selfDefault:Repr = {
    var e = this
    e = withDefault(k=>e)(newBuilder)
    e
  }

  
  /** tells if the two following method should work with no exception */
  def isNavigable:Boolean = false
  /** the parent of that PrefixTree ; this is not required and usually makes the implementation heavier */
  def parent:Repr = throw new NotImplementedError(s"parent is not accessible in that implementation: $getClass")
  /** the depth within the tree ; this is not required and usually makes the implementation heavier */
  def depth:Int   = throw new NotImplementedError(s"depth is not accessible in that implementation: $getClass")
  
  /** Removes a key from this tree, returning a new tree.
   *  @param    key the key to be removed
   *  @return   a new tree without a binding for `key`
   *
   *  @usecase  def - (key: K): Repr
   *    @inheritdoc
   */
  def - (key: K): Repr
  
  /** Adds a key/(value,tree) pair to this tree, returning a new tree.
   *  Enlarge subtrees without losing information is usualy the expected operation on trees.
   *  @param    kv the (key,subtree) pair
   *  @tparam   W the type of the value in the result tree
   *  @tparam   T the type of the result tree
   *  @return   a new tree with the new binding added to this tree
   *
   *  @usecase  def +[W,T] (kv: (K, T): T
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
  def apply(key: K): This = get(key) match {
    case None => if (default!=null) default(key) else params.noDefault(key)
    case Some(value) => value
  }
  
  /** As apply above, but for a succession of keys.
   */
  def apply(keys: K*): This = { var r=this; keys.foreach(x=>r=r(x)); r}
  
  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  override def isNonSignificant = value.isEmpty && isEmpty && default==null
  
  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable tree consisting only of those key where the key satisfies
   *          the predicate `p`. This results in a new tree in which keys that where
   *          removed now fall back on the default method.
   */
  def filterKeys(p: K => Boolean): This = filterAll(x => p(x._1))
  
  /** Filters this map by retaining only key/value satisfying a predicate.
   *  @param  p   the predicate used to test key/value
   *  @return an immutable tree consisting only of those items which satisfy
   *          the predicate `p`. This results in a new tree in which keys that
   *          were removed now fall back on the default method.
   */
  def filterAll(p: ((K,Repr)) => Boolean): This = {
    val bf = newBuilder
    if (!isEmpty) for (x:(K,Repr) <- this if p(x)) bf += ((x._1,x._2.filterAll(p)))
    bf.result(value,default)
  }
  
  /** Similar to the previous method, but the result is a view and doesn't rebuild
   *  a new tree. Such views are only useful when relatively few elements are used ;
   *  in other cases, it may be more performant to use filterAll. 
   */
  def filterView(p: ((K,Repr)) => Boolean): PrefixTreeLike[K,V,_] = new WithFilter(p)
  
  /** This class yields a filtered view of the current tree.
   *  Default apply and may be filtered : in that case they fall back on the noDefault method.
   *  Some operations will not work on views, in particular all methods that return Repr.
   *  A call to 'force' will create a full blown new Tree.
   */
  protected class WithFilter(p: ((K,Repr)) => Boolean) extends PrefixTreeLike[K,V,This#WithFilter] {
    type P = Nothing
    def -(key: K): This#WithFilter = ???
    override def default:K=>Repr = self.asDefault(k => { val r=self(k); if (p(k,r)) new r.WithFilter(p) else { val r=self.params.noDefault(k); new r.WithFilter(p) } })
    def get(key: K): Option[This#WithFilter] = self.get(key).filter(t=>p(key,t)).map(x => new x.WithFilter(p))
    def iterator: Iterator[(K, This#WithFilter)] = self.iterator.filter(p).map(x => (x._1,new x._2.WithFilter(p)))
    protected[this] def newBuilder: PrefixTreeLikeBuilder[K,V,This#WithFilter] = ???
    def params: Nothing = ???
    def update1[W >: V, T >: This#WithFilter <: PrefixTreeLike[K,W,T]](kv: (K, T))(implicit bf: PrefixTreeLikeBuilder[K,W,T]): T = ???
    def value: Option[V] = self.value
    def assoc:self.type = self
    /** Rebuilds the view as a true PrefixTreeLike. Defaults are again available, and removed keys will fall on default.
     */
    def force:This = {
      val bf = self.newBuilder
      if (!isEmpty) for (x <- this) bf += ((x._1,x._2.force))
      bf.result(self.value,self.default)
    }
  }

  /**  Returns the value associated with a key, or a default value if the key is not contained in the tree.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the tree.
   *   @tparam  T        the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *
   *   @usecase def getOrElse[T](key: K, default: => T): T
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
  
  /** Returns an iterable over the defined values.
   */
  def values:Iterable[Repr] = new DefaultValuesIterable

  /** Creates a new tree by combining this tree with a fallback tree.
   *  This builds a new tree, which keeps the value and default of this tree.
   *  This differs from using 'that' as default, in which case 'that' default would be used
   *  if the key was not found neither in 'this' nor 'that'.
   */
  def orElse[W>:V, T>:Repr<:PrefixTreeLike[K,W,T]](that: T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    bf(value,that ++ this,default)
  
  /** Transform this tree with another tree.
   *  Both trees are explored 'in parallel' and each sub-node of this tree is transformed using the
   *  corresponding sub-node of the transformer tree using the provided `op`.
   *  Default values for the second tree are used but exceptions will end the transformation ungracefully.
   *  The resulting tree contains an appropriate zipped default.
   *  @param t      the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree for defined
   *                values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault is an error or similar answer (e.g. empty)
   *  @param op     an operator that transforms a T node with a This node giving the expected U result
   *  @return the resulting transformed tree
   */
  override def zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:(T,Repr)=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:Repr):R = {
      val b=bf.newEmpty
      for (x:(K,This) <- cur)
        if (!strict || tt.isDefinedAt(x._1)) try { b += ((x._1, recur(tt(x._1),x._2))) } catch { case _:NoSuchElementException => }
      b.result(op(tt,cur),cur.asDefault(k=>recur(if (!strict || tt.isDefinedAt(k)) tt(k) else tt.params.noDefault(k),cur.default(k))))
    }
    recur(t,this)
  }
  
  /** Similar to the previous method, but the operation is provided through a third tree which is explored
   *  'in parallel' too.
   *  Default values for both trees are used but exceptions will end the transformation ungracefully.
   *  Note that this operation is one of the most complex for trees, and it could be used to define most other
   *  tree transformations that are defined here, albeit in a more costly way (performance wise.) ; for example
   *  zip above can be expressed here by using a constant op tree ; various map operations can be expressed by
   *  zipping a tree with itself etc...
   *  @param t   the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree or op tree for 
   *                defined values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault or O.noDefault is an error or similar answer (e.g. empty)
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node
   *  @return the resulting transformed tree
   */
  override def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:Repr,oo:O):R = {
      val b=bf.newEmpty
      for (x:(K,This) <- cur)
        if (strict.succeeds(tt,oo)(x._1)) try { b += ((x._1, recur(tt(x._1),x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
      b.result(oo.value.flatMap(_(tt,cur)),cur.asDefault(k=>recur(if (!strict.tree_strict || tt.isDefinedAt(k)) tt(k) else tt.params.noDefault(k),cur.default(k),if (!strict.op_strict || oo.isDefinedAt(k)) oo(k) else oo.params.noDefault(k))))
    }
    recur(t,this,op)
  }

  /** Transforms this tree by applying a function to every retrieved value.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f   the function used to transform the values of this tree.
   *  @return a tree which maps every element of this tree.
   *            The resulting tree is a new tree.
   */
  def map[W,T<:PrefixTreeLike[K,W,T]](f:V=>W)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = {
    if (!isEmpty) {
      //OK: one explanation here, but it is the same everywhere:
      //We must duplicate bf to have an empty copy to work on with children : the bf used at
      //this level is in used and cannot be shared with children!
      //However, that copy can be shared between chidren, because the result for one child is
      //built before we pass it on to the next child, and taking the result resets the builder.
      //working in this way lets us use much more performant ArrayBuffer[(K,T)] rather than
      //full blown maps (or whatever underlying structure is used in T) by using the
      //empty ++ ((k,t)) construct (which would work, but be awfully inefficient.)
      val bf1 = bf.newEmpty
      for (x <- this) bf += ((x._1,x._2.map(f)(bf1)))
    }
    bf.result(value.map(f),asDefault(default(_:K).map(f)))
  }
  
  /** The clone of this tree is the same tree using the same builder, cloning each sub-tree
   */
  override def clone:Repr = {
    val bf = newBuilder
    for (x <- this) bf += ((x._1,x._2.clone))    
    bf.result(value,default)
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
  def mapFull[L,W,T<:PrefixTreeLike[L,W,T]](f:(K=>L,L=>K,V=>W))(implicit bf:PrefixTreeLikeBuilder[L,W,T]):T = {
    if (!isEmpty) {
      val bf1 = bf.newEmpty
      bf ++= (for (x:(K,This) <- this) yield ((f._1(x._1),x._2.mapFull(f)(bf1))))
    }
    bf.result(value.map(f._3), asDefault((l:L)=>default(f._2(l)).mapFull(f)))
  }
  
  /** Transforms this tree by applying a function to every key.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f the function used to transform the keys of this tree.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree.
   */
  def mapKeys[L,W>:V,T<:PrefixTreeLike[L,W,T]](f:(K=>L,L=>K))(implicit bf:PrefixTreeLikeBuilder[L,W,T]):T = {
    if (!isEmpty) {
      val bf1 = bf.newEmpty
      bf ++= (for (x:(K,This) <- this) yield ((f._1(x._1),x._2.mapKeys[L,W,T](f)(bf1))))
    }
    bf.result(value, asDefault((l:L)=>default(f._2(l)).mapKeys[L,W,T](f)))
  }
  
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
    bf ++= e
    if (!isEmpty) {
      val bf1 = bf.newEmpty
      bf ++= (for (x:(K,This) <- this) yield ((x._1,x._2.flatMap[W,T](f)(bf1))))
    }
    bf.result(e.value, asDefault(default(_:K).flatMap[W,T](f)))
  }
  
  /** Creates a new tree obtained by updating this tree with a given key/value pair.
   *  @param    kv the key/value pair
   *  @tparam   L the type of the new keys
   *  @tparam   T the type of the added value
   *  @return   A new tree with the new key/value mapping added to this map.
   */
  def update1[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T
  
  /** Identical to the previous method, but more than one element is updated.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T)*)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = update[W,T](kv)
  
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
   *  @tparam   W  the type of the added values
   *  @tparam   T  the type of the added tree
   *  @return   a new tree with the given bindings added to this tree
   */
  def + [W>:V,T>:Repr<:PrefixTreeLike[K,W,T]] (kv1:(K,T), kv2:(K,T), kvs:(K,T) *)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    this +[W,T] kv1 + kv2 ++ kvs

  /** Adds all key/value pairs in a traversable collection to this tree, returning a new tree.
   *
   *  @param    xs  the collection containing the added key/value pairs
   *  @tparam   W  the type of the added values
   *  @tparam   T  the type of the added tree
   *  @return   a new tree with the given bindings added to this tree
   */
  def ++[W>:V, T >: Repr <: PrefixTreeLike[K,W,T]](xs: GenTraversableOnce[(K, T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    ((repr: T) /: xs.seq) (_ + _)

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
    this.iterator.map { case (k, v) => s"$k -> $v" }.addString(b, start, sep, end)

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
  type Gen[K,V] = PrefixTreeLike[K,V,_<:PrefixTreeLike[K,V,_]]
  
  implicit def toSeq[T<:PrefixTreeLike[_,_,T]](t:T):t.SeqView = t.seqView()
  
  sealed class NavigableMode(val id:Int)
  val nonNavigable    = new NavigableMode(0)
  val unsafeNavigable = new NavigableMode(1)
  val safeNavigable   = new NavigableMode(2)
  
  /** The minimum for building the Params used by the Tree implementation.
   */
  class Params[K,+V,+T<:PrefixTreeLike[K,V,T]] (
    /** The default method that will be used if no default is provided.
     *  It is strongly recommended that it either throws an exception or returns a
     *  non significant value.
     *  This method must not be understood as a fallback default, but as a handler
     *  for errors, that is any key not handled by the underlying iterable when
     *  there is not provided default.
     */
    val noDefault:K=>T,
    /** The tree will not contain non significant nodes.
     *  As a consequence, some defined sequences of keys that led to such values will disappear,
     *  and if invoked, they will fall back on the default.
     */
    val stripEmpty:Boolean,
    /** The tree is built with navigable elements.
     */
    val navigable:NavigableMode
  )
  
  /** An abstract class for the trait. Used to share code.
   */
  abstract class Abstract[K, +V, +This <: PrefixTreeLike[K, V, This]] extends AbstractPartialFunction[K, This] with PrefixTreeLike[K, V, This] { this:This=>
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
/*
trait CanBuildFrom[-From, -Elem, +To] {
// Creates a new builder
def apply(from: From): Builder[Elem, To]
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
*/

