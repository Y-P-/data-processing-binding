package utils.tree

import scala.collection.IterableLike
import scala.collection.MapLike
import scala.collection.AbstractSet
import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.collection.generic.Subtractable
import scala.collection.mutable.ArrayBuffer
import scala.runtime.AbstractPartialFunction
import scala.annotation.switch

/** Describes a tree where data is reached through a succession of keys.
 *  A tree can be viewed as a map where the values are themselves trees. However, the likeness to maps
 *  stops there, as the recursive nature of trees bring different properties.
 *
 *  One can distinguish three kinds of trees, with important topological differences.
 *  - 'canonical' : trees nodes are unique : no two nodes are memory wise identical
 *  - 'degenerated' : trees that contain duplicate nodes, but no infinite path
 *  - 'infinite' : trees that contain infinite paths, which can only be achieved by also being 'degenerated'
 *                 in that case, one can distinguish these trees where infinite path require the use of the
 *                 default function (type 1) and these which do not (type 2).
 *                 Some features that type 2 trees break:
 *                 * no flat representation
 *                 * no order for nodes
 *                 * no natural traversal
 *                 infinite-1 trees will perform correctly for most transformations,
 *                 however their computed default will be mostly wrong.
 *
 *  All of the methods perform as expected on 'canonical' trees.
 *  The identity semantic carried by 'degenerated' trees cannot be duplicated through transformations at a
 *  reasonable cost (e.g. if the degenerated tree is mapped, two memory identical nodes won't transform to
 *  two memory identical nodes, but into two different nodes that carry identical information)
 *  Trying to conserve the identity semantic is usually not useful, in addition to being costly.
 *  Infinite trees enter a quite different category because their topology class is different ; most tree
 *  transformations are ensured to loop endlessly (type 2), and while this can be avoided (at some cost), only the
 *  user can ensure that the algorithm does what he wants (it is not possible to prepare a generic algorithm
 *  that will easily cover all use case.)
 *
 *  The actual data of type V is optional in intermediary nodes, but a well formed tree should not have
 *  empty leaves. Using the methods with (GenTraversable[K],V) parameters (flat view of the tree) rely
 *  on that property (they will omit to output such degenerate branches, and will not build them.)
 *
 *  Operations on trees are not to take lightly ; the recursive nature of trees usually involves that
 *  any transformation be done by building the result from scratch, which may involve many computations.
 *
 *  Trees are rich structures, and the current definition barely scratches what can be done.
 *  The methods provided in this class, while useful, are mere common uses of the basic algorithm in PrefixLoop.
 *  New transformations may be required for handling specific, less common, cases : this is usually easy by
 *  picking up the right abstract class/traits from PrefixLoop and filling up the few missing methods.
 *
 *  Trees are made of two parts:
 *  - an iterable 'set' of (key,sub-tree)
 *  - a `default` method to handle keys not in the previous set
 *  Whenever an iteration is involved on the tree, `default` never applies.
 *  Whenever a transformation is done (`map` for example) , usually `default` is properly handled.
 *  `default` doesn't have to handle all possible keys. Whenever it doesn't handle a key, it can throw
 *  whatever exception, or `PrefixTreeLike.NoDefault[K]` which then calls another 'global' default method.
 *
 *  There are usually three kinds of trees, in ascending order of complexity, memory footprint, building speed:
 *  - Standard trees provide no way to ascend into the tree.
 *    It is strongly recommended to use these trees unless some navigation is absolutely necessary.
 *    Note that most methods that deal with trees (`foreach`, `fold`...) have a variant that hand the
 *    current path to the tree top as parameter (context) : this often makes it unnecessary to be able
 *    to actually navigate trees.
 *  - Weak Navigable trees allow access to the parent, BUT it relies on mutation and may sometimes be
 *    unsafe to use ; in particular, a tree that is built using shared branches may have incorrect parents.
 *    These trees can contain sub-parts that are not navigable.
 *  - Strong Navigable trees are just like weak navigable trees, but nodes are always rebuilt on assignment.
 *    In particular, these tree nodes always correctly point to the right parent. However, the building
 *    cost is usually expensive. Nodes are still mutable on the remove operation where the parent is reset
 *    to null.
 *
 *  Building a tree involves a parameter that specifies some of the implementation details:
 *  - `noDefault` fallback method to handle the PrefixTreeLike.NoDefault[K] exception
 *  - `stripEmpty` which specifies whether to keep degenerated nodes
 *  - `navigable` which spefies the navigability capabilities of the tree (see above)
 *
 *  The main difference between the methods in this class and the parent class is the way `default` is hanled.
 *  Here, an effort is made to specifically use the current tree default where this is possible.
 *  The methods of the parent class will not (cannot) do that effort and only provide generic means to
 *  build an appropriate default. Thus, when using trees where default matters, you must be careful to
 *  use the methods from this class, or use the appropriate `genMap...` method, or write your own method
 *  using the `buildDefault` method (as an example, you can read `flatMap`here)
 *
 *  Implementing a concrete class involves defining the following methods:
 *    Mandatory:
 *    `def iterator: Iterator[(K, Repr)]`
 *    `def get(key: K): Option[Repr]`
 *    `def value: Option[V]`
 *    `def update1[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T`
 *    `def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]`
 *
 *    Optional:
 *    `def default: K=>Repr = null`   //used to get a default value for a key not in the (K,This) collection
 *    `def - (key: K): Repr`          //can throw an exception as no internal method requires it
 *
 *    For classes that support upwards navigation (not recommended):
 *    `def isNavigable:Boolean`       //if your class supports navigation upwards
 *    `def parent:Repr`
 *    `def depth:Int`
 *
 *    You may consider overriding these methods for efficiency:
 *    `def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T])`
 *    `def size: Int`
 *    `def isEmpty: Boolean`
 *    `def foreach[U](f: ((K,Repr)) => U): Unit`
 *
 *    @tparam K the key type
 *    @tparam V the contained value type
 *    @tparam This the prefix tree type
 */
trait PrefixTreeLike[K, +V, +This <: PrefixTreeLike[K, V, This]]
  extends PartialFunction[K, This]
     with PrefixTraversableOnce[K, V, This]
     with Iterable[(K,This)]
     with IterableLike[(K, This), This]
     with Subtractable[K, This]
  { self:This =>

  import PrefixTreeLike._

  /** The general parameters used for building the tree.
   */
  type Params <: PrefixTreeLike.Params[K,V,Repr]
  def params:Params

  /** A new instance of builder similar to the one used to build this tree element.
   *  It can be used to build elements of the same tree kind.
   *  we must make a concrete implementation here or this `newBuilder` is not accounted for properly.
   */
  protected[this] override def newBuilder:PrefixTreeLikeBuilder[K,V,Repr] = ???

  /** Optionally returns the value associated with a key.
   *  The basic supposition is that keys found in the iterator yield a value, others yield a None.
   *
   *  @param  key    the key value
   *  @return an option value containing the value associated with `key` in this tree,
   *          or `None` if none exists.
   */
  def get(key: K): Option[Repr]

  /** The value for the current node */
  def value: Option[V]

  /** Defines the default value computation for the tree, returned when a key is not found.
   *  It can be null, in which case a fallback default (usually an exception) is used.
   *  This is assumed to throw `NoDefault` when it doesn't handle a given key so as to
   *  fallback on the noDefault handler.
   *
   *  @param key the given key value for which a binding is missing.
   */
  def default: K=>Repr = null

  /** Creates a new tree obtained by updating this tree with a given key/value pair.
   *  @param    kv the key/value pair
   *  @tparam   L the type of the new keys
   *  @tparam   T the type of the added value
   *  @return   A new tree with the new key/value mapping added to this map.
   */
  def update1[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T

  /** tells if the two following method should work with no exception */
  def isNavigable:Boolean = false
  /** the parent of that PrefixTree ; this is not required and usually makes the implementation heavier */
  def parent:Repr = throw new NotImplementedError(s"parent is not accessible in that implementation: $getClass")
  /** the depth within the tree ; this is not required and usually makes the implementation heavier */
  def depth:Int   = throw new NotImplementedError(s"depth is not accessible in that implementation: $getClass")


  //an internal utility to correctly rebuild default when necessary
  @inline final protected def asDefault[L,T](f:L=>T):L=>T = if (default==null) null else f

  /** The empty tree of the same type as this tree
   *  @return   an empty tree of type `This`.
   */
  def empty: Repr = self.newBuilder(None,Nil,null)
  override def seq: Repr = this
  override def toIterable:Repr = this

  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  override def isNonSignificant = value.isEmpty && isEmpty && default==null

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
  /** The clone of this tree is the same tree using the same builder, cloning each sub-tree
   */
  override def clone:Repr = {
    val bf = newBuilder
    for (x <- this) bf += ((x._1,x._2.clone))
    bf.result(value,default)
  }

  /** Retrieves the value which is associated with the given key. This
   *  method invokes the `default` method of the tree if there is no mapping
   *  from the given key to a value. Unless overridden, the `default` method
   *  automatically falls back on the `noDefault` which usually throws a
   *  `NoSuchElementException`.
   *
   *  @param  key the key
   *  @return     the value associated with the given key, or the result of the
   *              tree's `default` method, if none exists.
   */
  @throws(classOf[java.util.NoSuchElementException])
  def apply(key: K): This = get(key) match {
    case None => if (default!=null) try { default(key) } catch { case e:PrefixTreeLike.NoDefault[K] => params.noDefault(key) } else params.noDefault(key)
    case Some(value) => value
  }

  /** As apply above, but for a succession of `keys`.
   */
  @throws(classOf[java.util.NoSuchElementException])
  def apply(keys: K*): This = { var r=this; keys.foreach(x=>r=r(x)); r}

  /** As get, but for a succession of `keys` (no default applied.)
   */
  def get(keys:GenTraversableOnce[K]):Option[Repr] = keys.foldLeft[Option[Repr]](Some(self))((t,k)=>if (t==None) None else t.get.get(k))

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
  def filterAll(p: ((K,Repr)) => Boolean): This = filter(null.asInstanceOf[K])(p,x => x._2.default)(newBuilder)

  /** Similar to the previous method, but the result is a view and doesn't rebuild
   *  a new tree. Such views are only useful when relatively few elements are used ;
   *  in other cases, it may be more efficient to use filterAll.
   */
  def filterView(p: ((K,Repr)) => Boolean): PrefixTraversableOnce[K,V,_] = super.filterView(null.asInstanceOf[K])(p)

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

  /**  Returns the value associated with a key, or a default value if the key is not contained in the tree.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is found in the tree.
   *   @tparam  T        the result type of the default computation.
   *   @return  the value associated with `key` if it exists, otherwise the result of the `default` computation.
   */
  def getOrElse[T>:Repr](key: K, default: => T): T = get(key).getOrElse(default)

  /** Tests whether this tree contains a binding for a key.
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def contains(key: K): Boolean = get(key).isDefined

  /** Tests whether this tree contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`. The default function doesn't
   *  qualify for the success of `isDefinedAt` in the standard implementation.
   *  However it is possible to overload this method to account for the values
   *  covered by the default.
   *
   *  @param key the key
   *  @return `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def isDefinedAt(key: K): Boolean = contains(key)


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
    type Data = ((K,Repr),T)
    val values = (cur:Data)              => op(cur._2,cur._1._2)
    val next   = (child:(K,Repr),s:Data) => if (!strict || s._2.isDefinedAt(child._1)) try { s._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    genMap(null.asInstanceOf[K],t,values,next)
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
  override def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),(T,PrefixTreeLike.Tree[K,(T,Repr)=>Option[U]]))
    val v = (cur:Data) => cur._2._2.value.flatMap(_(cur._2._1,cur._1._2))
    val n = (child:(K,This),s:Data) => strict(child._1,s._2._1,s._2._2)
    genMap(null.asInstanceOf[K],(t,op),v,n)
  }

  /** As the previous method, but still more generic since the keys can also be changed.
   *  However, for the default to work properly, it is necessary to be able to go from a given L to the associated K (meaning
   *  the transformation K <-> L should be bijective) ; if this cannot be achieved, it is not possible to fall back on the
   *  appropriate default (deduced from the current node default), and instead the resulting tree will have no default.
   *  Of course, it is possible to make a yet more generic method (where the L=>K transformation would depend on the
   *  path, or where another default method could be user provided as a tree or whatever... The reader can probably write
   *  them himself once he understands the scheme underlying all of the tree transformations.
   */
  override def zipRec[W,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,W,R]](t:T,strict:Boolean,op:(Seq[((K,This),T)])=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]):R = {
    type Data = Seq[((K,This),T)]
    val values = (cur:Data)              => op(cur)
    val next   = (child:(K,Repr),s:Data) => if (!strict || s.head._2.isDefinedAt(child._1)) try { s.head._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    genMapRec(null.asInstanceOf[K],t,values,next)
  }

  /** As the previous method, but still more generic since the keys can also be changed.
   *  However, for the default to work properly, it is necessary to be able to go from a given L to the associated K (meaning
   *  the transformation K <-> L should be bijective) ; if this cannot be achieved, it is not possible to fall back on the
   *  appropriate default (deduced from the current node default), and instead the resulting tree will have no default.
   *  Of course, it is possible to make a yet more generic method (where the L=>K transformation would depend on the
   *  path, or where another default method could be user provided as a tree or whatever... The reader can probably write
   *  them himself once he understands the scheme underlying all of the tree transformations.
   */
  def zipFullRec[L,W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W]),O],R<:PrefixTreeLike[L,W,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W]),O],g:L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]):R = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data)              => cur.head._2._2.value.map(_(cur)).orNull
    val next   = (child:(K,This),s:Data) => strict(child._1,s.head._2._1,s.head._2._2)
    genMapRec(k0,(t,op),values,g,next)
  }

  /** Transforms this tree by applying a function to every retrieved value.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f   the function used to transform the values of this tree.
   *  @return a tree which maps every element of this tree.
   *            The resulting tree is a new tree.
   */
  def map[W,R<:PrefixTreeLike[K,W,R]](f:V=>W)(implicit bf:PrefixTreeLikeBuilder[K,W,R]):R =
    genMap(null.asInstanceOf[K],(x:(K,This))=>x._2.value.flatMap(z=>Some(f(z))))


  /** A map operation that preserves the memory identity semantics of the original tree.
   *  Use this only for 'infinite' trees, or 'degenerate' trees that require such semantics.
   *  Defaults are ignored.
   */
  def xmap[L,W,R<:MutablePrefixTreeLike[L,W,R]](fk:K=>L,fv:V=>W)(implicit bf: PrefixTreeLikeBuilder[L,W,R]):R = {
    //here we keep the list of keys to the node where a child is not yet filled up ; the key for that child ; a way to retrieve the result later
    var todo = List[(Seq[L],L,()=>R)]()
    //processes duplicate entries
    val process : (Boolean,Option[(L,R)],Seq[(K,This)],This=>Option[(L,R)])=>Option[(L,R)] = (immediate,r,ctx,fetch) => r match {
      //the result is not yet ready : keep the current item state in store for later
      case None    => val keys = ctx.tail.map(x=>fk(x._1)).reverse.tail        //ctx(0) is the missing key, ctx(last) is the virtual top key -null-
                      val l = fk(ctx.head._1)
                      todo = ((keys,l,()=>fetch(ctx.head._2).get._2)) +: todo  //get will know the result!
                      Some((l,bf.empty))                                       //prepare the entry to preserve node order ; set it with a placeholder at this time
      //here we already have the result : forward it (only the node value is kept, the key from the original entry is of no interest!)
      case Some(x) => Some(fk(ctx.head._1),x._2)
    }
    //main loop, discard default
    val r = (new PrefixLoop.RecurRecTreeNoData[K,V,L,W,R,This] {
      def reverseKey: L=>K = null
      def mapValues(ctx: Context): Values = (fk(ctx.head._1),ctx.head._2.value.map(fv),null)
    })((null.asInstanceOf[K],this), process)
    //once finished, all dangling references should exist!
    for (x <- todo) r(x._1:_*)(x._2) = x._3()  //assign unknown nodes by calling the method 'which knows the appropriate result'!
    r
  }

  /** A copy operation that preserves the memory identity semantics of the original tree.
   *  Use this only for 'infinite' trees, or 'degenerate' trees that require such semantics.
   *  Defaults are ignored.
   */
  def xcopy[W>:V,R<:MutablePrefixTreeLike[K,W,R]](implicit bf: PrefixTreeLikeBuilder[K,W,R]):R =
    xmap[K,W,R](identity,identity)


  /** Iterates through an 'infinite' tree.
   *  While 'f' is called on each node (even duplicated ones), children of duplicated
   *  nodes are not explored.
   *  As all methods deeling with 'infinite' trees, this is costly and should be avoided
   *  when you know that your tree is not 'infinite'.
   *  You may also wish to use this method too on 'degenerate' trees if you don't want
   *  to explore duplicated nodes more than once.
   */
  def xdeepForeachRec(k0:K)(f:(Seq[(K,This)],=>Unit)=>Any):Unit =
    PrefixLoop.loopRec[K,V,This](f)((k0,this),(immediate,r,ctx,fetch) => { if (immediate) f(ctx,()); None })
  def xdeepForeach(k0:K)(f:((K,This),=>Unit)=>Any):Unit =
    PrefixLoop.loop[K,V,This](f)((k0,this),(immediate,r,ctx,fetch) => { if (immediate) f(ctx,()); None })

  /** A fold operation for infinite trees, with the same limitations as above.
   */
  def xdeepFoldLeft[U](u0:U,k0:K,topFirst:Boolean)(f: (U, (K,This)) => U): U =
    PrefixLoop.fold(u0,topFirst,f)(xdeepForeach(k0) _)
  def xdeepFoldLeftRec[U](u0:U,k0:K,topFirst:Boolean)(f: (U, Seq[(K,This)]) => U): U =
    PrefixLoop.fold(u0,topFirst,f)(xdeepForeachRec(k0) _)

  /** Transforms this tree by applying a function to every retrieved value and key.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f the function used to transform the keys and values of this tree.
   *            it must be bijective in K<=>L if there are any default involved,
   *            and in that case, f._2 must be given ; otherwise it can be null,
   *            bijectivity is not compulsory but defaults are ignored.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree.
   */
  def mapFull[L,W,T<:PrefixTreeLike[L,W,T]](k0:K,fk:K=>L,fv:V=>W,g:L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,T]):T =
    genMap(k0,(x:(K,This))=>(fk(x._1),x._2.value.flatMap(z=>Some(fv(z)))),g)


  /** Transforms this tree by applying a function to every key.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f the function used to transform the keys of this tree.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree.
   */
  def mapKeys[L,W>:V,T<:PrefixTreeLike[L,W,T]](k0:K,fk:K=>L,g:L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,T]):T =
    genMap[L,W,T](k0,(x:(K,This))=>(fk(x._1),x._2.value),g)

  /** Merging with another tree.
   *  merge does the following to build the children:
   *  - if the merged tree is not significant, the result is this tree ; otherwise
   *  - check all the elements in the current tree
   *    * if the current key is not present in the merged tree, copy (the child is the child from this tree)
   *    * if it is present and overwrite is true, copy from the merged tree (the child is the child from the merged tree)
   *    * if it is present and overwrite is false, merge both children
   *  - pour all the children from the merged tree that have no corresponding key in this tree (additional children from the merged tree)
   *  In order to build the merged node value and default (which doesn't apply for children that were copied!) :
   *  - use the value and default from the merged tree
   *  The result is a mix between both trees, as can be expected.
   *  Defaults are not merged : the most appropriate default from either tree is used!
   *  @param r the tree which is merged onto this tree
   *  @param overwrite whose effect is described above
   *  @param mergeValues which computes the resulting value from the current value and the merged node value
   *  @param useDefault is true if the merge also uses the default values of the merged tree when a key in this
   *                    tree has no correspondence in the merged tree
   *  @tparam L the key kind for the merged tree
   *  @tparam W the value kind for the merged tree
   *  @tparam R the tree kind for the merged tree
   */
  def merge[L>:K,W>:V,R>:This<:PrefixTreeLike[L,W,R]](r:R,overwrite:Boolean,mergeValues:(Option[W],Option[W])=>Option[W],useDefault:Boolean)(implicit bf: PrefixTreeLikeBuilder[L,W,R]):R = {
    if      (r eq this)          this  //if merging with oneself (comparing references), return self
    else if (isNonSignificant)   r     //if this is empty, the merge is r
    else (new PrefixLoop.ExpandTree[R,K,V,L,W,R,This] {
      protected def buildDefault(ctx:Context,default:K => This): L => R = ???  //unused
      protected def merge(ctx:Context,r1:(L,R),r2:(L,R)):R = if (overwrite) r2._2 else r1._2.merge(r2._2,overwrite,mergeValues,useDefault)
      protected def mapValues(ctx:Context): Values = (ctx._1._1, mergeValues(ctx._1._2.value,ctx._2.value), ctx._2.default, ctx._2)
      protected def nextX(child:(K,This),ctx:Context): R = ctx._2.get(child._1) match {
        case None     => if (useDefault) try { ctx._2(child._1) } catch { case _:NoSuchElementException=>child._2 } else child._2 //if nothing to merge with : merge with self
        case Some(r1) => r1            //merge with found branch
      }
    })(((null.asInstanceOf[K],this),r))
  }

  /** Transforms this tree by applying a function on all values contained in a tree, which returns
   *  a tree. This tree is expanded recursively : the result is a new tree.
   *  It works also on default values.
   *  This is a non intuitive transformation that should be handled with care.
   *  Cases of conflict on expansion (i.e. the tree resulting from applying the function contains
   *  children with the same keys) are solved by using a merge strategy.
   *  Note: this is best used to expand trees in which only leaves have values: in that case
   *        the transformation makes sense:
   *        - intermediate nodes are unaffected (no value to expand)
   *        - terminal nodes are expanded downwards with the trees attached to the held value
   *
   *  @param  f the function used to transform the values of this tree.
   *  @param  mode defines how conflicts are resolved:
   *          - KEEP ignores the newly expended tree conflicting child in favor of the existing child
   *          - REPLACE does the reverse
   *          - MERGE merges the newly expanded tree on the existing child
   *          - MERGE_OVERWRITE merges the newly expanded tree on the existing child with overwrite on merge
   *          - MERGE_REVERSE does as MERGE, but reverses the roles of the tress
   *          - MERGE_REVERSE_OVERWRITE does as MERGE_OVERWRITE, but reverses the roles of the tress
   *          This parameter can be null if no conflict is expected.
   *  @param useDefault indicates whether one should use the default when a merge is involved
   *  @return a tree which maps every value of this tree to a new tree with same key type
   *            The resulting tree is a new tree with the same key type, the new value type,
   *            which expands this tree with new subtrees.
   */
  def flatMap[W,T<:PrefixTreeLike[K,W,T]](mode:MergeMode)(f:V=>T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = {
    (new PrefixLoop.ExpandTreeNoData[K,V,K,W,T,This] with PrefixLoop.SameKeyDefault {
      protected def merge(ctx:Context,r1:(L,R),r2:(L,R)):R = {
        if (mode==null) throw new IllegalStateException("merge required: MergeMode cannot be null")
        mode[L,W,T](r1._2,r2._2,(u,v)=>v)
      }
      protected def mapValues(ctx:Context): Values = {
        val e = (if (ctx._2.value.isDefined) f(ctx._2.value.get) else bf.empty)
        (ctx._1, e.value, buildDefault(ctx,ctx._2.default), e)
      }
    })((null.asInstanceOf[K],this))
  }

  /** Same as above, but the transformation works on a recursive context rather than simply on the node value.
   */
  def flatMap[L,W,T<:PrefixTreeLike[L,W,T]](k0:K,mode:MergeMode,g:L=>K)(f:Seq[(K,This)]=>(L,T))(implicit bf:PrefixTreeLikeBuilder[L,W,T]):(L,T) = {
    (new PrefixLoop.ExpandRecTreeNoData[K,V,L,W,T,This] with PrefixLoop.OtherKeyDefault {
      def reverseKey = g
      protected def merge(ctx:Context,r1:(L,R),r2:(L,R)):R = {
        if (mode==null) throw new IllegalStateException("merge required: MergeMode cannot be null")
        mode[L,W,T](r1._2,r2._2,(u,v)=>v)
      }
      protected def mapValues(ctx:Context): Values = {
        val e = f(ctx)
        (e._1, e._2.value, buildDefault(ctx,ctx.head._2.default), e._2)
      }
    }).get((k0,this))
  }

  /** Partitions this tree in two trees according to a predicate.
   *
   *  @param k0 a key for this tree.
   *  @param p the predicate on which to partition.
   *  @return  a pair of trees: the first tree contains all values from elements that
   *           satisfy the predicate `p` and the second tree contains all values from
   *           elements that don't. The relative order of the elements in the resulting
   *           trees is the same as in the original tree.
   *           Both trees may contain overlapping nodes (with the same key sequence),
   *           but only one of these will contain a value.
   *           Defaults are removed from the result.
   */
  def partition(k0:K)(p: ((K,This)) => Boolean): (Repr, Repr) = (new PrefixLoop.BasicNoDataRB with PrefixLoop.Iterate[Nothing,(This,This),K,V,This] {
    protected def deepLoop(ctx:Context, loop: ((Result,Context)=>Any)=>Unit):Result = {
      val l, r = newBuilder                                            //prepare buffer for accepted/non accepted nodes
      loop { (x,c)=> l+=((c._1,x._1)); r+=((c._1,x._2)) }              //shuffle partitioned children between both buffers with the right key
      if (p(ctx)) (l.result(ctx._2.value,null), r.result(None,null))   //build this result if accepted
      else        (l.result(None,null),  r.result(ctx._2.value,null))  //build this result if rejected
    }
  }).get((k0,this))

  def partitionRec(k0:K)(p: (Seq[(K,This)]) => Boolean): (Repr, Repr) = (new PrefixLoop.RecNoDataRB with PrefixLoop.Iterate[Nothing,(This,This),K,V,This] {
    protected def deepLoop(ctx:Context, loop: ((Result,Context)=>Any)=>Unit):Result = {
      val l, r = newBuilder                                                 //prepare buffer for accepted/non accepted nodes
      loop { (x,c)=> l+=((c.head._1,x._1)); r+=((c.head._1,x._2)) }         //shuffle partitioned children between both buffers with the right key
      if (p(ctx)) (l.result(ctx.head._2.value,null), r.result(None,null))   //build this result if accepted
      else        (l.result(None,null),  r.result(ctx.head._2.value,null))  //build this result if rejected
    }
  }).get((k0,this))


  /** Identical to the previous method, but more than one element is updated.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T)*)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    update[W,T](kv)

  /** Identical to the previous method, but elements are passed through an iterable like rather than a built Seq.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    kv.foldLeft[T](repr)(_.update[W,T](_))

  /** Adds a key/(value,tree) pair to this tree, returning a new tree.
   *  Enlarge subtrees without losing information is usualy the expected operation on trees.
   *  @param    kv the (key,subtree) pair
   *  @tparam   W the type of the value in the result tree
   *  @tparam   T the type of the result tree
   *  @return   a new tree with the new binding added to this tree
   *
   *  @usecase  def +[W,T] (kv: (K, T): T
   */
  def + [W>:V,T>:Repr<:PrefixTreeLike[K,W,T]] (kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
    update[W,T](kv)

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
  def +[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]] (kv1:(K,T), kv2:(K,T), kvs:(K,T) *)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T =
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

  /* Overridden for efficiency. */
  override def toSeq: Seq[(K, Repr)] = toBuffer[(K, Repr)]
  override def toBuffer[C >: (K, Repr)]: ArrayBuffer[C] = {
    val result = new ArrayBuffer[C](size)
    copyToBuffer[C](result)
    result
  }

  /** Tries to turn a one level deep hierarchy to a map.
   *  This will fail if any subtree exists.
   *  Empty nodes (with no value) are skipped.
   *  Order is preserved.
   *  @param b a builder for the expected map result
   *  @tparam M the expected map kind
   */
  def toMap[W>:V,M<:Map[K, W]](implicit b:scala.collection.mutable.Builder[(K, V), M]):M = {
    for (v <- this) {
      if (!v._2.isEmpty) throw new IllegalStateException("cannot transform hierarchy to map if not exactly 1 level deep")
      if (v._2.value!=None) b += ((v._1,v._2.value.get))
    }
    b.result
  }

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every tree is `"Map"`.
   */
  override def stringPrefix: String = value match {
    case None    => if (default==null) "Tree" else "Tree*"
    case Some(v) => if (default==null) "Tree{"+v+"}" else "Tree*{"+v+"}"
  }

  // Common generic transformations on PrefixTreeLike ; they all build a PreficTreeLike

  import PrefixLoop._

  @throws(classOf[NoSuchElementException])
  def genMap[W, R<:PrefixTreeLike[K,W,R]](k0:K,v:((K, Repr))=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurTreeNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default))
  })((k0,this))

  @throws(classOf[NoSuchElementException])
  def genMap[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:(((K, Repr), X))=>Option[W], x:((K, Repr),((K, Repr), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurTreeSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default))
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

  @throws(classOf[NoSuchElementException])
  def genMapRec[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:Seq[(K, Repr)]=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecTreeNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default))
  })((k0,this))

  @throws(classOf[NoSuchElementException])
  def genMapRec[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>Option[W], x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecTreeSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default))
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

  @throws(classOf[NoSuchElementException])
  def genMap[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:((K, This))=>(L, Option[W]), g : L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurTreeNoData[K,V,L,W,R,This] {
    def reverseKey = g
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default)) }
  })((k0,this))

  @throws(classOf[NoSuchElementException])
  def genMap[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W]), g: L=>K, x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurTree[X,K,V,L,W,R,This] {
    def reverseKey = g
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default)) }
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

  @throws(classOf[NoSuchElementException])
  def genMapRec[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:Seq[(K, This)]=>(L, Option[W]), g : L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecTreeNoData[K,V,L,W,R,This] {
    def reverseKey = g
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default)) }
  })((k0,this))

  @throws(classOf[NoSuchElementException])
  def genMapRec[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W]), g : L=>K, x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecTree[X,K,V,L,W,R,This] {
    def reverseKey = g
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default)) }
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

}

object PrefixTreeLike {
  //a usefull type for helping the compiler in type inference : use this for parameters and avoid generics on O
  type Tree[K,V] = PrefixTreeLike[K,V,O] with O forSome { type O<:PrefixTreeLike[K,V,O] }

  //use this in a default method if you want to fall back on the default 'no default' method.
  class NoDefault[K](val key:K) extends Exception

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
     *  for errors, that is any key not handled either by the underlying iterable or
     *  the provided default (exception NoDefault) or there is not provided default.
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
