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
 *  The actual data of type V is optional in intermediary nodes, but a well formed tree should not have
 *  empty leaves. Using the methods with (GenTraversable[K],V) parameters (flat view of the tree) rely
 *  on that property (they will omit to output such degenerate branches, and will not build them.)
 *
 *  Operations on trees are not to take lightly ; the recursive nature of trees usually involves that
 *  any transformation be done by rebuilding it. This is always done by using the generic recursive
 *  algorithm in PrefixLoop, derived as an appropriate class. The methods provided in this class, while
 *  useful, are mere uses of this algorithm. In many cases, achieving a specific transformation will imply
 *  rewriting a given method in a slightly different way (e.g. in flatMap you might want mergeMode to
 *  depend on the context, or be handled by a parallel tree) ; this is usually easy by picking up the
 *  right abstract class/traits from PrefixLoop and filling up the few missing methods.
 *
 *  PrefixTreeLike seems to share a lot of functionalities with MapLike.
 *  However, it doesn't seem possible to derive from that trait because of the constraints put on This.
 *  We end up with generic signatures that slightly differ from MapLike ones, such as
 *  `[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]]` instead of a simple `[T>:Repr]`, and these are not compatible.
 *  In any case, a PrefixTree may look like a map, but it is not a map!
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
 *  Implementing a concrete class involves defining the following methods:
 *    Mandatory:
 *    `def iterator: Iterator[(K, Repr)]`
 *    def get(key: K): Option[Repr]
 *    def value: Option[V]
 *    def update1[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T
 *    def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]`
 *
 *    Optional:
 *    def default: K=>Repr = null   //used to get a default value for a key not in the (K,This) collection
 *    def - (key: K): Repr          //can throw an exception as no internal method requires it
 *
 *    For classes that support upwards navigation (not recommended):
 *    def isNavigable:Boolean       //if your class supports navigation upwards
 *    def parent:Repr
 *    def depth:Int
 *
 *    You may consider overriding these methods for efficiency:
 *    def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTreeLikeBuilder[K,W,T])
 *    def size: Int
 *    def isEmpty: Boolean
 *    def foreach[U](f: ((K,Repr)) => U): Unit
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
  { self:This =>

  import PrefixTreeLike._

  /** The general parameters used for building the tree.
   */
  type Params <: PrefixTreeLike.Params[K,V,Repr]
  def params:Params

  /** A new instance of builder similar to the one used to build this tree element.
   *  It can be used to build elements of the same tree kind.
   */
  protected[this] override def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]

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
   *  This is assumed to throw `NoSuchElementException` when it doesn't handle a given key.
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
  def seq: Repr = this
  override def toIterable = iterator.toIterable  //scala library does a wrong cast here

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
   *  from the given key to a value. Unless overridden, the `default` method throws a
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
   *  in other cases, it may be more performant to use filterAll.
   */
  def filterView(p: ((K,Repr)) => Boolean): This = super.filter(null.asInstanceOf[K])(p,x => x._2.default)(newBuilder)

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

  /** This class yields a filtered view of the current tree.
   *  Default apply and may be filtered : in that case they fall back on the noDefault method.
   *  Some operations will not work on views, in particular all methods that return Repr.
   *  A call to 'force' will create a full blown new Tree of `This` type..
   */
  protected class WithFilter(p: ((K,Repr)) => Boolean) extends PrefixTreeLike.Abstract[K,V,This#WithFilter] {
    type P = Nothing
    //useless
    def -(key: K): This#WithFilter = ???
    def params: Nothing = ???
    protected[this] def newBuilder: PrefixTreeLikeBuilder[K,V,This#WithFilter] = ???
    def update1[W >: V, T >: This#WithFilter <: PrefixTreeLike[K,W,T]](kv: (K, T))(implicit bf: PrefixTreeLikeBuilder[K,W,T]): T = ???
    //implementation
    override def default:K=>Repr = self.asDefault(k => { val r=self(k); if (p(k,r)) new r.WithFilter(p) else { val r=self.params.noDefault(k); new r.WithFilter(p) } })
    def get(key: K): Option[This#WithFilter] = self.get(key).filter(t=>p(key,t)).map(x => new x.WithFilter(p))
    def iterator: Iterator[(K, This#WithFilter)] = self.iterator.filter(p).map(x => (x._1,new x._2.WithFilter(p)))
    def value: Option[V] = self.value
    def force:This = {
      val bf = self.newBuilder
      if (!isEmpty) for (x <- this) bf += ((x._1,x._2.force))
      bf.result(self.value,self.default)
    }
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
   *  @param useDefault is true if the merge also uses the default values of the merged tree when a key in this
   *                    tree has no correspondence in the merged tree
   *  @tparam L the key kind for the merged tree
   *  @tparam W the value kind for the merged tree
   *  @tparam R the tree kind for the merged tree
   */
  def merge[L>:K,W>:V,R>:This<:PrefixTreeLike[L,W,R]](r:R,overwrite:Boolean,useDefault:Boolean)(implicit bf: PrefixTreeLikeBuilder[L,W,R]):R = {
    if      (r eq this)          this  //if merging with oneself (comparing references), return self
    else if (isNonSignificant)   r     //if this is empty, the merge is r
    else (new PrefixLoop.ExpandTree[R,K,V,L,W,R,This] {
      protected def buildDefault(ctx:Context,default:K => This,g: L => K): L => R = ???  //unused
      protected def merge(ctx:Context,r1:(L,R),r2:(L,R)):R = if (overwrite) r2._2 else r1._2.merge(r2._2,overwrite,useDefault)
      protected def mapValues(ctx:Context): Values = (ctx._1._1, ctx._2.value, ctx._2.default, ctx._2.toIterable)
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
   *  children with the same keys) is solved by using a merge strategy.
   *  Note: this is best used to expand trees with only leaves with values: in that case
   *        the transformation makes sense:
   *        - intermediate nodes are unaffected (no value to expand)
   *        - terminal nodes are expanded downward with the trees attached to the held value
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
   *            which expands this tree values to new subtrees.
   */
  def flatMap[W,T<:PrefixTreeLike[K,W,T]](mode:MergeMode, useDefault:Boolean)(f:V=>T)(implicit bf:PrefixTreeLikeBuilder[K,W,T]):T = {
    (new PrefixLoop.ExpandTreeNoData[K,V,K,W,T,This] with PrefixLoop.SameKeyDefault {
      protected def merge(ctx:Context,r1:(L,R),r2:(L,R)):R = {
        if (mode==null) throw new IllegalStateException("merge required: MergeMode cannot be null")
        mode[L,W,T](r1._2,r2._2,useDefault)
      }
      protected def mapValues(ctx:Context): Values = {
        val e = (if (ctx._2.value.isDefined) f(ctx._2.value.get) else bf.empty)
        (ctx._1, e.value, buildDefault(ctx,ctx._2.default,null), e.toIterable)
      }
    })((null.asInstanceOf[K],this))
  }

  /** Same as above, but the transformation works on a recursive context rather than simply on the node value.
   */
  def flatMap[L,W,T<:PrefixTreeLike[L,W,T]](k0:K,mode:MergeMode,useDefault:Boolean,g:L=>K)(f:Seq[(K,This)]=>(L,T))(implicit bf:PrefixTreeLikeBuilder[L,W,T]):(L,T) = {
    (new PrefixLoop.ExpandRecTreeNoData[K,V,L,W,T,This] with PrefixLoop.OtherKeyDefault {
      protected def merge(ctx:Context,r1:(L,R),r2:(L,R)):R = {
        if (mode==null) throw new IllegalStateException("merge required: MergeMode cannot be null")
        mode[L,W,T](r1._2,r2._2,useDefault)
      }
      protected def mapValues(ctx:Context): Values = {
        val e = f(ctx)
        (e._1, e._2.value, buildDefault(ctx,ctx.head._2.default,g), e._2.toIterable)
      }
    }).get((k0,this))
  }

  /** Identical to the previous method, but more than one element is updated.
   */
  def update[W>:V,T>:Repr<:PrefixTreeLike[K,W,T]](kv:(K,T)*)(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = update[W,T](kv)

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
  def + [W>:V,T>:Repr<:PrefixTreeLike[K,W,T]] (kv:(K,T))(implicit bf:PrefixTreeLikeBuilder[K,W,T]): T = update[W,T](kv)

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

  /* Overridden for efficiency. */
  override def toSeq: Seq[(K, Repr)] = toBuffer[(K, Repr)]
  override def toBuffer[C >: (K, Repr)]: ArrayBuffer[C] = {
    val result = new ArrayBuffer[C](size)
    copyToBuffer[C](result)
    result
  }

  override def stringPrefix: String = super[PrefixTraversableOnce].stringPrefix

  // Common generic transformations on PrefixTreeLike ; they all build a PreficTreeLike

  import PrefixLoop._

  @throws(classOf[NoSuchElementException])
  def genMap[W, R<:PrefixTreeLike[K,W,R]](k0:K,v:((K, Repr))=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurTreeNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default,null))
  })(k0,this)

  @throws(classOf[NoSuchElementException])
  def genMap[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:(((K, Repr), X))=>Option[W], x:((K, Repr),((K, Repr), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurTreeSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default,null))
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

  @throws(classOf[NoSuchElementException])
  def genMapRec[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:Seq[(K, Repr)]=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecTreeNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default,null))
  })(k0,this)

  @throws(classOf[NoSuchElementException])
  def genMapRec[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>Option[W], x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecTreeSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values = (v(ctx), buildDefault(ctx,getCurrent(ctx)._2.default,null))
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

  @throws(classOf[NoSuchElementException])
  def genMap[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:((K, This))=>(L, Option[W]), g : L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurTreeNoData[K,V,L,W,R,This] {
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default,g)) }
  })(k0,this)

  @throws(classOf[NoSuchElementException])
  def genMap[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W]), g: L=>K, x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurTree[X,K,V,L,W,R,This] {
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default,g)) }
    def nextX(child: (K, This), ctx: Context): X = x(child,ctx)
  })(((k0,this),x0))

  @throws(classOf[NoSuchElementException])
  def genMapRec[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:Seq[(K, This)]=>(L, Option[W]), g : L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecTreeNoData[K,V,L,W,R,This] {
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default,g)) }
  })(k0,this)

  @throws(classOf[NoSuchElementException])
  def genMapRec[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W]), g : L=>K, x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecTree[X,K,V,L,W,R,This] {
    def mapValues(ctx: Context): (L, Option[W], L=>R) = { val z=v(ctx); (z._1,z._2,buildDefault(ctx,getCurrent(ctx)._2.default,g)) }
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

