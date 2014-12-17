package utils.tree

/** This object provides methods that are used in the package to iterate through trees.
 *
 *  There are 2 classe of iterations:
 *  - these that use a global function to apply on all nodes
 *  - these that use a companion tree that provides for each node a companion function to apply on that node
 *  There are also two sub-classes:
 *  - these that require being able to reach into the parents (e.g. if you are writing the global key for each node)
 *  - these that only use the current node information
 *  Iterating into a tree also brings problem of its own; when reaching a node, you may want to (not exclusive):
 *  - do something before iterating on the chidren
 *  - do something after having iterated on the children
 *
 *  In addition, in a tree, usual filtering operations have to be adapted: filtering can depend on the position
 *  of an element in the tree, not only its value. This is unlike usual collections where most often position is
 *  not significant : the place of an object in the tree carries semantic. We assume that the easiest way to carry
 *  such semantic is through 'zip like' operations, where a companion tree is unwound in parallel with the current
 *  tree : each companion node can bear relevant information to apply to the current node. In general, the following
 *  rule is assumed:
 *  - if a companion value is None, then that node is 'skipped', but not its children
 *  - if it is Some(null), then that node and children are skipped
 *  - if it is Some(x), then iteration proceeds normally on that node
 *  Note that not invoking the child recursion (which is usually provided as a =>Unit parameter) is the way for a
 *  node to process itself but not its children.
 *
 *  All of these cases have to be handled, which account in a large measure to the apparent complexity of this object.
 */
object PrefixLoop {

  /** Top trait for defining the generic recursion framework on PrefixIterableOnce.
   *
   *  It lets do most recursive calls that apply to trees, including map-like or
   *  even flatMap-like transformations. The sheer number of possible combinations
   *  will not make it possible to give a variant of each possible uses, which are
   *  almost limitless.
   *
   *  It is configurable by mixing in the appropriate subtraits.
   *  Most 'internal' methods are made final on mixin to ensure that only compatible
   *  are mixed together. The traits are 'orthogonal', in that they each define a set
   *  of the required methods : in the end, a number of traits must be mixed together
   *  to get a working class (or other implementations for the missing methods provided.)
   *
   *  Usually, if the provided traits are used (or the appropriate combination given as
   *  an abstract class below), then the user only has to concentrate on two methods:
   *  - `def mapValues(ctx:Context):Values` which is the transformation itself (and must
   *     call `recur` in some way.)
   *  - `def nextX(child:(K,This),ctx:Context):X` which, when external data is required
   *    for the transformation, provides the next layer of data for a given child within
   *    a given context.
   *
   *  @tparam X some kind of parameter data provided by the user
   *  @tparam K the key type for the tree we are working on
   *  @tparam V the value type for the tree we are working on
   *  @tparam L the key type for the tree we are converting to
   *  @tparam W the value type for the tree we are converting to
   *  @tparam R the type of the tree we are converting to
   *  @tparam This the type of the tree we are working on
   */
  trait RecurBase {
    //user type
    type X
    //target tree parameter types
    type K
    type V
    type This <: PrefixTraversableOnce[K, V, This]
    //advance declaration to prevent lots of override : may be used for anything, but usually to accept a Tree declaration
    type L
    type W
    type R

    type Data     //Some basic information pertaining to the current layer
    type Context  //usually either Data (we don't need parents info) or Seq[Data] (we need parents info)
    type Values   //some kind of data required to build the result
    type Result   //Result produced by the recursion : null value is possible but is reserved (skipped node)

    /** this builds the `values` from a given context; very user dependent */
    protected def mapValues(ctx:Context):Values
    /** this builds the user data for a given context and child; very user dependent */
    protected def nextX(child:(K,This),ctx:Context):X

    /** given some Values and a Context, builds the converted result and associated key
     * @param ctx the current context
     */
    protected def buildResult(ctx:Context,v:Values,loop:(Result=>Any) => Unit):Result

    //usually provided by a rather generic trait (`RecWithDataRB` etc)
    /** this is used to start the recursion from the top layer : it builds the initial context */
    protected def initialize(d0:Data):Context
    /** current element in the transformed tree */
    protected def getCurrent(ctx:Context):(K,This)
    /** given some Data (a child layer) and Context, builds the Context for that child */
    protected def childContext(child:Data,ctx:Context):Context
    /** given some child and parent context, builds the Result for that child */
    protected def mapChild(child:(K,This),ctx:Context):Result

    /** This builds a method to loop on the children in the given context.
     *  @param ctx the context on which children we are looping
     *  @return a method that can be called to loop over all children.
     *          it accepts a method as parameter, that will be called for all children if it is not null.
     */
    @inline private final def childrenLoop(ctx:Context): (Result=>Any) => Unit = (g:Result=>Any) => for (z <- getCurrent(ctx)._2) {
      val r=mapChild(z, ctx)
      if (g!=null && r!=null) g(r)
    }
    /** Applies the recursion on all the tree layers below ctx
     *  @param ctx the context on which children we are looping
     *  @return the Result for that ctx
     */
    final protected def recur(ctx: Context):Result = mapValues(ctx) match {
      case null => null.asInstanceOf[Result]
      case u    => buildResult(ctx,u,childrenLoop(ctx))
    }
    /** Applies the transformation
     *  @param d0, the initial layer
     *  @return the Result of the transformation
     */
    @throws(classOf[NoSuchElementException])
    final def get(d0:Data): Result = recur(initialize(d0)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  //traits for cases with (or without) specific X data
  trait NoDataRB extends RecurBase {
    override type X = Nothing
    type Data = (K,This)
    final def nextX(child:(K,This),ctx:Context):Nothing = ???
    @inline final def mapChild(child:(K,This),ctx:Context):Result = childContext(child,ctx) match {
      case null => null.asInstanceOf[Result]
      case d    => recur(d)
    }
  }
  trait WithDataRB extends RecurBase {
    type Data = ((K,This),X)
    @inline final protected def mapChild(child:(K,This),ctx:Context):Result = nextX(child, ctx) match {
      case null => null.asInstanceOf[Result]
      case x1   => childContext((child,x1),ctx) match {
        case null => null.asInstanceOf[Result]
        case d    => recur(d)
      }
    }
  }
  trait WithDisjointDataRB extends RecurBase {
    type Data = (K,This)
    @inline final protected def mapChild(child:(K,This),ctx:Context):Result = nextX(child, ctx) match {
      case null => null.asInstanceOf[Result]
      case x1   => childContext(child,ctx) match {
        case null => null.asInstanceOf[Result]
        case d    => recur(d)
      }
    }
  }

  //traits for accessing (or not) parents
  trait BasicRB extends RecurBase {
    type Context = Data
    @inline final protected def childContext(child:Data,ctx:Context):Context = child
    @inline final protected def initialize(d0:Data):Context = d0
  }
  trait ParentsRB extends RecurBase {
    type Context = Seq[Data]
    @inline final protected def childContext(child:Data,ctx:Context):Context = child +: ctx
    @inline final protected def initialize(d0:Data):Context = Seq(d0)
  }

  /** These trait define how the current element is retrieved: this depends on the presence of X and
   *  on the Context type.
   */
  trait RecWithDataRB extends WithDataRB with ParentsRB {
    @inline final def getCurrent(ctx:Context):(K,This) = ctx.head._1
  }
  trait RecNoDataRB extends NoDataRB with ParentsRB {
    @inline final protected def getCurrent(ctx:Context):(K,This) = ctx.head
  }
  trait BasicWithDataRB extends WithDataRB with BasicRB {
    @inline final protected def getCurrent(ctx:Context):(K,This) = ctx._1
  }
  trait BasicNoDataRB extends NoDataRB with BasicRB {
    @inline final protected def getCurrent(ctx:Context):(K,This) = ctx
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///                     Useful in mapLike transformations                        ///
  ////////////////////////////////////////////////////////////////////////////////////

  trait MapLikeRB[XX,KK,VV,LL,WW,TT<:PrefixTraversableOnce[KK,VV,TT]] extends RecurBase {
    type X=XX
    type K=KK
    type V=VV
    type L=LL
    type W=WW
    type R<:PrefixTraversableOnce[L,W,R]    //easy internal reference
    type This=TT
    type Result=(L,R)
    /** fetches the W value for the current node result */
    protected def getValue(v:Values):Option[W]
    /** fetches the L key for the current node result */
    protected def getResultKey(v:Values,ctx:Context):L
    /** Running against the current tree. */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): R = get(d0)._2
  }

  //traits for building either View or Tree
  trait ViewRB[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends MapLikeRB[X,K,V,L,W,T] {
    type R = PrefixTraversableOnce.Abstract[L,W]
    protected def buildResult(ctx:Context,v:Values,loop:(Result=>Any) => Unit):Result =
      (getResultKey(v,ctx),new R(getValue(v)) { def foreach[X](g:Result=>X):Unit = loop(g) })
  }
  trait TreeRB[X,K,V,L,W,R0<:PrefixTreeLike[L,W,R0],T<:PrefixTraversableOnce[K,V,T]] extends MapLikeRB[X,K,V,L,W,T] {
    type R=R0
    protected def bf:PrefixTreeLikeBuilder[L,W,R]
    protected def getDefault(v:Values):L=>R
    protected def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R

    final protected def stdDefault(ctx:Context,child:(K,This)):R = mapChild(child,ctx) match {
      case null => throw new PrefixTreeLike.NoDefault(child._1)
      case r    => r._2
    }

    protected def buildResult(ctx:Context,v:Values,loop:(Result=>Any) => Unit):Result = {
      val b = bf.newEmpty
      loop { b += _ }
      (getResultKey(v,ctx),b.result(getValue(v),getDefault(v)))
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///              Useful in transformations that create new nodes                 ///
  ////////////////////////////////////////////////////////////////////////////////////

  trait ExpandLikeRB[XX,KK,VV,LL,WW,TT<:PrefixTraversableOnce[KK,VV,TT]] extends RecurBase {
    type X=XX
    type K=KK
    type V=VV
    type L=LL
    type W=WW
    type R<:PrefixTraversableOnce[L,W,R]    //easy internal reference
    type This=TT
    type Result=(L,R)
    /** fetches the W value for the current node result */
    protected def getResultNodes(v:Values):Iterable[(L,Option[W],L=>R)]
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): R = get(d0)._2
  }

  //traits for building either View or Tree
  trait ExpandViewRB[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ExpandLikeRB[X,K,V,L,W,T] {
    type R = PrefixTraversableOnce.Abstract[L,W]
    protected def buildResult(ctx:Context,v:Values,loop:(Result=>Any) => Unit):Result =
      (getResultKey(v,ctx),new R(getValue(v)) { def foreach[X](g:Result=>X):Unit = loop(g) })
  }
  trait ExpandTreeRB[X,K,V,L,W,R0<:PrefixTreeLike[L,W,R0],T<:PrefixTraversableOnce[K,V,T]] extends ExpandLikeRB[X,K,V,L,W,T] {
    type R=R0
    protected def bf:PrefixTreeLikeBuilder[L,W,R]
    protected def getDefault(v:Values):L=>R
    protected def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R
    protected def overwrite:Boolean

    final protected def stdDefault(ctx:Context,child:(K,This)):R = mapChild(child,ctx) match {
      case null => throw new PrefixTreeLike.NoDefault(child._1)
      case r    => r._2
    }

    protected def buildResult(ctx:Context,v:Values,loop:(Result=>Any) => Unit):Result = {
      val b = bf.newEmpty
      val r = getResultNodes(v)
      if (!overwrite) for (x<-r) b+=x
      loop { b += _ }
      (getResultKey(v,ctx),b.result(getValue(v),getDefault(v)))
    }
  }
  
  /** Handling defaults when building trees: we want to provide a way to fall back on the original
   *  tree defaults. If the original is only traversable, overriding getDefault will likely be
   *  more usefull. Falling back on the original default requires to be able to navigate back
   *  to the original keys (i.e. K <-> L must be bijective)
   */
  trait SameKeyDefault[X,K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]] extends TreeRB[X,K,V,K,W,R,T] {
    override type L=K
    final protected def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R = l => stdDefault(ctx,(l,defa(l)))
  }
  trait OtherKeyDefault[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]] extends TreeRB[X,K,V,L,W,R,T] {
    final protected def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R = if (g==null) null else l => {
      val k = g(l)
      stdDefault(ctx,(k,defa(k)))
    }
  }

  /** These traits define common uses for the Values type.
   */
  trait ValueW extends RecurBase {
    override type L=K
    type Values = Option[W]
    @inline final protected def getValue(v:Values):Option[W] = v
    @inline final protected def getResultKey(v:Values,ctx:Context):L = getCurrent(ctx)._1
  }
  trait ValueWD extends RecurBase {
    override type L=K
    type Values = (Option[W], L=>R)
    @inline final protected def getValue(v:Values):Option[W] = v._1
    @inline final protected def getDefault(v:Values):L=>R = v._2
    @inline final protected def getResultKey(v:Values,ctx:Context):L = getCurrent(ctx)._1
  }
  trait ValueLW extends RecurBase {
    type Values = (L, Option[W])
    @inline final protected def getValue(v:Values):Option[W] = v._2
    @inline final protected def getResultKey(v:Values,ctx:Context):L = v._1
  }
  trait ValueLWD extends RecurBase {
    type Values = (L, Option[W], L=>R)
    @inline final protected def getValue(v:Values):Option[W] = v._2
    @inline final protected def getDefault(v:Values):L=>R = v._3
    @inline final protected def getResultKey(v:Values,ctx:Context):L = v._1
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///       Useful in iterative transformations (deepForeach, fold...)             ///
  ////////////////////////////////////////////////////////////////////////////////////

  trait IterLikeRB[XX,R0,KK,VV,TT<:PrefixTraversableOnce[KK,VV,TT]] extends RecurBase {
    type X=XX
    type K=KK
    type V=VV
    type This=TT
    type Result=R0
    type Values = Context
    protected def deepLoop(ctx:Context,loop:(Result=>Any) => Unit):Result
    //no conversions ; do nothing but don't return null (ignored in the loop)
    final protected def mapValues(ctx:Context):Values = ctx
    def buildResult(ctx:Context,v:Values,loop:(Result=>Any) => Unit):Result = deepLoop(ctx,loop)
    /** Running against the current tree. */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): Result = get(d0)
  }

  /** Actual classes for deep iteration in the tree.
   */
  abstract class LoopRecWithData[X,R0,K,V,T<:PrefixTraversableOnce[K,V,T]] extends IterLikeRB[X,R0,K,V,T] with RecWithDataRB
  abstract class LoopWithData[X,R0,K,V,T<:PrefixTraversableOnce[K,V,T]] extends IterLikeRB[X,R0,K,V,T] with BasicWithDataRB
  abstract class LoopRecNoData[R0,K,V,T<:PrefixTraversableOnce[K,V,T]] extends IterLikeRB[Nothing,R0,K,V,T] with RecNoDataRB
  abstract class LoopNoData[R0,K,V,T<:PrefixTraversableOnce[K,V,T]] extends IterLikeRB[Nothing,R0,K,V,T] with BasicNoDataRB
  /**
   * This class provides a way iterates through the tree while unwinding O in parallel and applying f to all elements
   */
  class ZipRec[K,V,O<:PrefixTreeLike[K,_,O],T<:PrefixTraversableOnce[K,V,T]](f:(Seq[((K,T),O)],=>Unit)=>Any) extends LoopRecWithData[O,Unit,K,V,T] {
    protected def nextX(child:(K,This), ctx:Context) = try { ctx.head._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }
    protected def deepLoop(ctx:Context,loop:(Result=>Any)=>Unit):Result = f(ctx,loop(null))
  }
  /**
   * builds a tuned up version of ZipRec.
   * @param h a method to transform F (often a function) into the function of two arguments expected by ZipRec:
   *          - Seq[((K,This),O)], the Context that is built while unwinding the trees
   *          - =>Unit, the Recursion call that is built while unwinding the trees
   *          And which returns Unit
   *          It is assumed that:
   *          - if a node in O contains None, then the recursion proceeds, but (obviously), the current node is skipped
   *          - if a node in O contains Some(null), then the current node and its children are skipped
   *          - otherwise, the recursion proceeds normally
   * @tparam K the key type
   * @tparam V the value type
   * @tparam This the tree type
   * @tparam F some data to work on
   * @tparam O the tree type for F
   *
   * @return a ZipRec that satisfies all the previous conditions
   */
  def zipRec[K,V,F,O<:PrefixTreeLike[K,F,O],This<:PrefixTraversableOnce[K,V,This]](h:F=>(Seq[((K,This),O)],=>Unit)=>Unit) =
    new ZipRec[K,V,O,This](zipBase(h,_.head._2.value))

  //Same as above, but f doesn't reach to the parents
  class Zip[K,V,O<:PrefixTreeLike[K,_,O],T<:PrefixTraversableOnce[K,V,T]](f:(((K,T),O),=>Unit)=>Any) extends LoopWithData[O,Unit,K,V,T] {
    protected def nextX(child:(K,This), ctx:Context) = try { ctx._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }
    protected def deepLoop(ctx:Context,loop:(Result=>Any)=>Unit):Result = f(ctx,loop(null))
  }
  //builds a tuned up version of Zip by providing a function and a tree of F ; the function is applied to F to get an appropriate function
  //to use in the transformation. F is often itself a function. See uses in deepForeachZip
  def zip[K,V,F,O<:PrefixTreeLike[K,F,O],This<:PrefixTraversableOnce[K,V,This]](h:F=>(((K,This),O),=>Unit)=>Unit) =
    new Zip[K,V,O,This](zipBase(h,_._2.value))

  //Useful for recursive iterative operations
  def fold[U,Context](u0:U,topFirst:Boolean,f: (U, Context) => U)(g:((Context, =>Unit) => Unit) => Unit):U = {
    var u = u0
    g(if (topFirst) (ctx,recur) => { u=f(u,ctx); recur } else (ctx,recur) => { recur; u=f(u,ctx) })
    u
  }
  def fold[U,Context,X](u0:U,topFirst:Boolean,extract:Context=>X)(g:(((U, X) => U) => ((Context, => Unit) => Unit)) => Unit) = {
    var u = u0
    g(x => if (x==null) null else if (topFirst) (ctx,recur) => { u=x(u,extract(ctx)); recur } else (ctx,recur) => { recur; u=x(u,extract(ctx)) })
    u
  }
  /** Generalizes the method to loop through a tree.
   */
  def loop[Context,X](extract:Context=>X)(g:(((X , => Unit) => Any) => ((Context, => Unit) => Unit)) => Unit):Unit =
    g(x => if (x==null) null else (ctx,recur) => x(extract(ctx),recur))
  
  /** Defines the general behavior when zipping:
   *  - if we have None as companion, ignore the current node but process the children
   *  - if we have Some(null), ignore the current node and its children
   *  - otherwise, process he current node and its children
   *  This is used mostly to use companion trees for zip traversals, that don't contain functions on the
   *  full context, but functions on a part of the context (usually only the keys or the (key,this) pair.)
   *  For example in PrefixTraversableOnce.deepForeachZip the functions work only on (K,This) instead of
   *  ((K,This),O), and this often makes much more sense (but O has to be in the context to be unwound in
   *  parallel with This)
   *  
   *  @param h     is a transformation from the companion tree element to a function usable in the transformation
   *  @param value indicates how the F value is fetched from the Context
   *  @return a function usable in Zip/ZipRec
   *  @tparam Context is the context type for the kind of transformation involved
   *  @tparam F is the kind of value in the companion tree
   *  @see zip,zipRec for use
   */
  def zipBase[Context,F](h:F=>((Context,=>Unit)=>Unit),value:Context=>Option[F]):(Context,=>Unit)=>Unit = (ctx,recur)=>value(ctx) match {
    case None    => recur
    case Some(g) => if (g!=null) h(g)(ctx,recur)
  }


  ////////////////////////////////////////////////////////////////////////////////////
  ///                    Classes for mapLike transformations                       ///
  ////////////////////////////////////////////////////////////////////////////////////
    
  /** Actual classes for all possible combinations.
   *  - with or without an X user data
   *  - with or without access to the parents
   *  - using same key as input or transforming the key
   *  - building Views (PrefixTraversableOnce) or Trees (PrefixTreeLike)
   *
   *  Most of the methods involved are quite short and we can expect the JIT compiler to inline them.
   *  This explains why these classes may be useful : apart from making things easier for the user (why
   *  force him to use a Context of Seq[Data] when he doesn't require access to the parents, or why force
   *  him to return the same key when he doesn't expect to transform the keys ?), it can be expected that
   *  the simpler classes should run quite faster for being (after inlining) much simpler!
   *
   *  This would have to be verified with a real performance test though.
   */
  abstract class RecurRecView[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[X,K,V,L,W,T] with RecWithDataRB with ValueLW
  abstract class RecurRec[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R])  extends OtherKeyDefault[X,K,V,L,W,R,T] with RecWithDataRB with ValueLWD
  abstract class RecurRecViewNoData[K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[Nothing,K,V,L,W,T] with RecNoDataRB with ValueLW
  abstract class RecurRecNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R])  extends OtherKeyDefault[Nothing,K,V,L,W,R,T] with RecNoDataRB with ValueLWD
  abstract class RecurView[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[X,K,V,L,W,T] with BasicWithDataRB with ValueLW
  abstract class Recur[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends OtherKeyDefault[X,K,V,L,W,R,T] with BasicWithDataRB with ValueLWD
  abstract class RecurViewNoData[K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[Nothing,K,V,L,W,T] with BasicNoDataRB with ValueLW
  abstract class RecurNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R])  extends OtherKeyDefault[Nothing,K,V,L,W,R,T] with BasicNoDataRB with ValueLWD
  abstract class RecurRecViewSameKey[X,K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[X,K,V,K,W,T] with RecWithDataRB with ValueW
  abstract class RecurRecSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R])  extends SameKeyDefault[X,K,V,W,R,T] with RecWithDataRB with ValueWD
  abstract class RecurRecViewNoDataSameKey[K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[Nothing,K,V,K,W,T] with RecNoDataRB with ValueW
  abstract class RecurRecNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R])  extends SameKeyDefault[Nothing,K,V,W,R,T] with RecNoDataRB with ValueWD
  abstract class RecurViewSameKey[X,K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[X,K,V,K,W,T] with BasicWithDataRB with ValueW
  abstract class RecurSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R]) extends SameKeyDefault[X,K,V,W,R,T] with BasicWithDataRB with ValueWD
  abstract class RecurViewNoDataSameKey[K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends ViewRB[Nothing,K,V,K,W,T] with BasicNoDataRB with ValueW
  abstract class RecurNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R])  extends SameKeyDefault[Nothing,K,V,W,R,T] with BasicNoDataRB with ValueWD
}