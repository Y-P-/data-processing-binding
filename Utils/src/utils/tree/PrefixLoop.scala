package utils.tree

object PrefixLoop {

  /** Top trait for defining the generic recursion framework on PrefixIterableOnce.
   *
   *  It lets do most recursive calls that build new trees, including map-like or
   *  even flatMap-like transformations. The sheer number of possible combinations
   *  will not make it possible to give a variant of each possible uses, which are
   *  almost limitless.
   *
   *  It is configurable but mixing in the appropriate subtraits.
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
    type X
    type K
    type V
    type L
    type W
    type R<:PrefixTraversableOnce[L,W,R]    //easy internal reference
    type This <: PrefixTraversableOnce[K, V, This]

    type Data     //Some basic information pertaining to the current layer
    type Context  //usually either Data (we don't need parents info) or Seq[Data] (we need parents info)
    type Values   //some kind of data required to build the result

    /** this builds the converted values from a given context; very user dependant */
    def mapValues(ctx:Context):Values
    /** this builds the user data for a given context and child; very user dependant */
    def nextX(child:(K,This),ctx:Context):X

    /** current element in the transformed tree */
    def getCurrent(ctx:Context):(K,This)
    /** fetches the key from the Values result */
    def getKey(v:Values):L
    /** fetches the value from the Values result */
    def getValue(v:Values):Option[W]
    /** this is used to start the recursion from the top layer : it builds the initial context */
    def initialize(d0:Data):Context
    /** given some Values and a Context, builds the converted result and associated key; calls recur. */
    def buildResult(l:L,w:Option[W],v:Values,loop:(((L,R))=>Any) => Unit):(L,R)
    /** given some Data (a child layer) and Context, builds the Context for that child */
    def childContext(child:Data,ctx:Context):Context
    /** given some Data (a child layer) and Context, builds the Context for that child */
    def mapChild(child:(K,This),ctx:Context):(L,R)
    /** key for a result */
    def getResultKey(v:Values,ctx:Context):L

    /** This builds a loop on the children in Context on which a given method are applied */
    @inline final def childrenLoop(ctx:Context): (((L,R))=>Any) => Unit = (g:((L,R))=>Any) => for (z <- getCurrent(ctx)._2) {
      val r=mapChild(z, ctx)
      if (r!=null) g(r)
    }
    /** Applies the recursion on all the tree layers */
    final def recur(ctx: Context):(L,R) = mapValues(ctx) match {
      case null => null
      case u    => buildResult(getResultKey(u,ctx),getValue(u),u,childrenLoop(ctx))
    }

    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): R = get(d0)._2

    @throws(classOf[NoSuchElementException])
    final def get(d0:Data): (L,R) = recur(initialize(d0)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  //traits for cases with (or without) specific X data
  trait NoDataRB extends RecurBase {
    override type X = Nothing
    type Data = (K,This)
    final def nextX(child:(K,This),ctx:Context):Nothing = ???
    @inline final def mapChild(child:(K,This),ctx:Context):(L,R) = childContext(child,ctx) match {
      case null => null
      case d    => recur(d)
    }
  }
  trait WithDataRB extends RecurBase {
    type Data = ((K,This),X)
    @inline final def mapChild(child:(K,This),ctx:Context):(L,R) = nextX(child, ctx) match {
      case null => null
      case x1   => childContext((child,x1),ctx) match {
        case null => null
        case d    => recur(d)
      }
    }
  }

  //traits for accessing (or not) parents
  trait BasicRB extends RecurBase {
    type Context = Data
    @inline final def childContext(child:Data,ctx:Context):Context = child
    @inline final def initialize(d0:Data):Context = d0
  }
  trait ParentsRB extends RecurBase {
    type Context = Seq[Data]
    @inline final def childContext(child:Data,ctx:Context):Context = child +: ctx
    @inline final def initialize(d0:Data):Context = Seq(d0)
  }

  //traits for building either View or Tree
  trait ViewRB[XX,KK,VV,LL,WW,TT<:PrefixTraversableOnce[KK,VV,TT]] extends RecurBase {
    type X=XX
    type K=KK
    type V=VV
    type L=LL
    type W=WW
    type This=TT
    type R = PrefixTraversableOnce.Abstract[L,W]
    def buildResult(l:L,w:Option[W],v:Values,loop:(((L,R))=>Any) => Unit):(L,R) =
      (l,new R(w) { def foreach[X](g:((L,R))=>X):Unit = loop(g) })
  }
  trait TreeRB[XX,KK,VV,LL,WW,RR<:PrefixTreeLike[LL,WW,RR],TT<:PrefixTraversableOnce[KK,VV,TT]] extends RecurBase {
    type X=XX
    type K=KK
    type V=VV
    type L=LL
    type W=WW
    type R=RR
    type This=TT
    def bf:PrefixTreeLikeBuilder[L,W,R]
    def getDefault(v:Values):L=>R
    def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R

    final def stdDefault(ctx:Context,child:(K,This)) = mapChild(child,ctx) match {
      case null => throw new PrefixTreeLike.NoDefault(child._1)
      case r    => r._2
    }

    @inline final def buildTreeElement(l:L,w:Option[W],v:Values,b:PrefixTreeLikeBuilder[L,W,R]) =
      (l,b.result(w,getDefault(v)))
    def buildResult(l:L,w:Option[W],v:Values,loop:(((L,R))=>Any) => Unit):(L,R) = {
      val b = bf.newEmpty
      loop { b += _ }
      buildTreeElement(l,w,v,b)
    }
  }

  /** Handling defaults when building trees: we want to provide a way to fall back on the original
   *  tree defaults. If the original is only traversable, overriding getDefault will likely be
   *  more usefull. Falling back on the original default requires to be able to navigate back
   *  to the original keys (i.e. K <-> L must be bijective)
   */
  trait SameKeyDefault[X,K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]] extends TreeRB[X,K,V,K,W,R,T] {
    override type L=K
    def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R = l => stdDefault(ctx,(l,defa(l)))
  }
  trait OtherKeyDefault[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]] extends TreeRB[X,K,V,L,W,R,T] {
    def buildDefault(ctx:Context,defa:K=>This,g:L=>K): L=>R = if (g==null) null else l => {
      val k = g(l)
      stdDefault(ctx,(k,defa(k)))
    }
  }

  /** These traits define common uses for the Values type.
   */
  trait ValueW extends RecurBase {
    override type L=K
    type Values = Option[W]
    @inline final def getKey(v:Values):L = ???
    @inline final def getValue(v:Values):Option[W] = v
    @inline def getResultKey(v:Values,ctx:Context):L = getCurrent(ctx)._1
  }
  trait ValueWD extends RecurBase {
    override type L=K
    type Values = (Option[W], L=>R)
    @inline final def getKey(v:Values):L = ???
    @inline final def getValue(v:Values):Option[W] = v._1
    @inline final def getDefault(v:Values):L=>R = v._2
    @inline def getResultKey(v:Values,ctx:Context):L = getCurrent(ctx)._1
  }
  trait ValueLW extends RecurBase {
    type Values = (L, Option[W])
    @inline final def getKey(v:Values):L = v._1
    @inline final def getValue(v:Values):Option[W] = v._2
    @inline def getResultKey(v:Values,ctx:Context):L = v._1
  }
  trait ValueLWD extends RecurBase {
    type Values = (L, Option[W], L=>R)
    @inline final def getKey(v:Values):L = v._1
    @inline final def getValue(v:Values):Option[W] = v._2
    @inline final def getDefault(v:Values):L=>R = v._3
    @inline def getResultKey(v:Values,ctx:Context):L = v._1
  }

  /** These trait define how the current element is retrieved: this depends on the presence of X and
   *  on the Context type.
   */
  trait RecWithDataRB extends WithDataRB with ParentsRB {
    @inline final def getCurrent(ctx:Context):(K,This) = ctx.head._1
  }
  trait RecNoDataRB extends NoDataRB with ParentsRB {
    @inline final def getCurrent(ctx:Context):(K,This) = ctx.head
  }
  trait BasicWithDataRB extends WithDataRB with BasicRB {
    @inline final def getCurrent(ctx:Context):(K,This) = ctx._1
  }
  trait BasicNoDataRB extends NoDataRB with BasicRB {
    @inline final def getCurrent(ctx:Context):(K,This) = ctx
  }

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