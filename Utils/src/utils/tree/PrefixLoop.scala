package utils.tree

object PrefixLoop {

  /** The most generic class that highlights the recursion algorithm on PrefixTraversableOnce.
   *  Most methods will use it in derived form to achieve their own purpose.
   *
   *  The methods `mapValues` and `nextX` define what the recursion do.
   *  The methods `initialize`, `buildResult`, `children`, `childContext`, `mapChild` are usually
   *  tied to the kind of algorithm used (are we building a tree, a traversable ? are we
   *  using user data or not ? are we using the same keys on input and output ?...)
   *
   *  @tparam X some kind of parameter data provided by the user
   *  @tparam K the key type for the tree we are working on
   *  @tparam V the value type for the tree we are working on
   *  @tparam L the key type for the tree we are converting to
   *  @tparam W the value type for the tree we are converting to
   *  @tparam Res  the type of the tree we are converting to
   *  @tparam This the type of the tree we are working on
   */
  trait RecurBase[X, K, V, L, W, Res<:PrefixTraversableOnce[L,W,Res], This <: PrefixTraversableOnce[K, V, This]] {
    type Data     //Some basic information pertaining to the current layer
    type Context  //usually either Data (we don't need parents info) or Seq[Data] (we need parents info)
    type Values   //some kind of data required to build the result
    type R=Res    //easy internal reference

    /** this builds the converted values from a given context; very user dependant */
    def mapValues(ctx:Context):Values
    /** this builds the user data for a given context and child; very user dependant */
    def nextX(child:(K,This),ctx:Context):X

    /** fetches the key from the Values result */
    def getKey(v:Values):L
    /** fetches the value from the Values result */
    def getValue(v:Values):Option[W]
    /** this is used to start the recursion from the top layer : it builds the initial context */
    def initialize(d0:Data):Context
    /** given some Values and a Context, builds the converted result and associated key; calls recur. */
    def buildResult(v:Values,loop:(((L,R))=>Any) => Unit):(L,R)
    /** given some Context, builds the traversable for its children */
    def children(ctx:Context):TraversableOnce[(K,This)]
    /** given some Data (a child layer) and Context, builds the Context for that child */
    def childContext(child:Data,ctx:Context):Context
    /** given some Data (a child layer) and Context, builds the Context for that child */
    def mapChild(child:(K,This),ctx:Context):(L,R)

    /** This builds a loop on the children in Context on which a given method are applied */
    final def childrenLoop(ctx:Context): (((L,R))=>Any) => Unit = (g:((L,R))=>Any) => for (z <- children(ctx)) {
      val r=mapChild(z, ctx)
      if (r!=null) g(r)
    }
    /** Applies the recursion on all the tree layers */
    final def recur(ctx: Context):(L,R) = mapValues(ctx) match {
      case null => null
      case u    => buildResult(u,childrenLoop(ctx))
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

  trait NoDataRB[K, V, L, W, R<:PrefixTraversableOnce[L,W,R], This <: PrefixTraversableOnce[K, V, This]]
        extends RecurBase[Nothing,K,V,L,W,R,This] {
    type Data = (K,This)
    final def nextX(child:(K,This),ctx:Context):Nothing = ???
    @inline final def mapChild(child:(K,This),ctx:Context):(L,R) = childContext(child,ctx) match {
      case null => null
      case d    => recur(d)
    }
  }
  trait WithDataRB[X,K, V, L, W, R<:PrefixTraversableOnce[L,W,R], This <: PrefixTraversableOnce[K, V, This]]
        extends RecurBase[X,K,V,L,W,R,This] {
    type Data = ((K,This),X)
    @inline final def mapChild(child:(K,This),ctx:Context):(L,R) = nextX(child, ctx) match {
      case null => null
      case x1   => childContext((child,x1),ctx) match {
        case null => null
        case d    => recur(d)
      }
    }
  }
  trait BasicRB[X,K, V, L, W, R<:PrefixTraversableOnce[L,W,R], This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,L,W,R,This] {
    type Context = Data
    @inline final def childContext(child:Data,ctx:Context):Context = child
  }
  trait ParentsRB[X,K, V, L, W, R<:PrefixTraversableOnce[L,W,R], This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,L,W,R,This] {
    type Context = Seq[Data]
    @inline final def childContext(child:Data,ctx:Context):Context = child +: ctx
  }
  trait ViewRB[X,K, V, L, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,L,W,PrefixTraversableOnce.Abstract[L,W],This] {
    @inline final def buildResult(v:Values,loop:(((L,R))=>Any) => Unit):(L,R) = {
      (getKey(v),new R(getValue(v)) { def foreach[X](g:((L,R))=>X):Unit = loop(g) })
    }
  }
  trait TreeRB[X, K, V, L, W, Res <: PrefixTreeLike[L,W,Res], This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,L,W,Res,This] {
    def bf:PrefixTreeLikeBuilder[L,W,R]
    def buildDefault(cur:Context,defa:K=>This,g:L=>K): L=>R

    def buildTreeElement(v:Values,b:PrefixTreeLikeBuilder[L,W,R]) = (v._1,b.result(v._2,v._3))
    @inline final def buildResult(v:Values,loop:(((L,R))=>Any) => Unit):(L,R) = {
      val b    = bf.newEmpty
      loop { b += _ }
      buildTreeElement(v,b)
    }
  }

  /** The most generic class that highlights the recursion algorithm on PrefixTraversableOnce.
   *  Most methods will use it in derived form to achieve their own purpose.
   *  It has the following properties:
   *  - user methods mapValues and nextX can refer to the whole caller hierarchy,
   *    up to the top of the tree if necessary
   *  - user methods mapValues and nextX can both return null ; such a result indicates that
   *    the currently processed node will be ignored (and its children too) ; either acts as
   *    an embedded filter. If the top tree meets that filter condition, the final apply
   *    method (which builds the transformed tree) will throw NoSuchElementException.
   *  Because this recursion builds many intermediate objects (sequence of parents, pairs
   *  of ((K,This),X), and calls methods that are useless in many cases (e.g. when K=L or U=V)
   *  Other simpler and more efficient forms are provided.
   */
  abstract class RecurRecView[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data    = ((K, This), X)
    type Context = Seq[Data]
    type Values  = (L, Option[W])
    type R       = PrefixTraversableOnce.Abstract[L,W]

    def mapValues(ctx:Context):Values
    def nextX(child:(K,This),ctx:Context):X

    def children(ctx:Context):TraversableOnce[(K,This)]      = ctx.head._1._2
    def childContext(child:((K,This),X),ctx:Context):Context = child +: ctx

    def mapChild(child:(K,This),ctx:Context):(L,R) = nextX(child, ctx) match {
      case null => null
      case x1   => childContext((child,x1),ctx) match {
        case null => null
        case d    => recur(d)
      }
    }

    final def childrenLoop[X](s:Context): (((L,R))=>X) => Unit = (g:((L,R))=>X) => for (z <- children(s)) {
      val r=mapChild(z, s)
      if (r!=null) g(r)
    }
    final def recur(ctx: Context):(L,R) = mapValues(ctx) match {
      case null => null
      case u    => buildResult(u,ctx)
    }

    def buildResult(v:Values,ctx:Context) = {
      val loop = childrenLoop(ctx)
      (v._1,new R(v._2) { def foreach[X](g:((L,R))=>X):Unit = loop(g) })
    }

    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): R = get(d0)._2

    @throws(classOf[NoSuchElementException])
    final def get(d0:Data): (L,R) = recur(Seq(d0)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  /** The most generic class that highlights the recursion algorithm on PrefixTraversableOnce.
   *  Most methods will use it in derived form to achieve their own purpose.
   *  It has the following properties:
   *  - user methods mapValues and nextX can refer to the whole caller hierarchy,
   *    up to the top of the tree if necessary
   *  - user methods mapValues and nextX can both return null ; such a result indicates that
   *    the currently processed node will be ignored (and its children too) ; either acts as
   *    an embedded filter. If the top tree meets that filter condition, the final apply
   *    method (which builds the transformed tree) will throw NoSuchElementException.
   *  Because this recursion builds many intermediate objects (sequence of parents, pairs
   *  of ((K,This),X), and calls methods that are useless in many cases (e.g. when K=L or U=V)
   *  Other simpler and more efficient forms are provided.
   */
  abstract class RecurRec[X, K, V, L, W, R0 <: PrefixTreeLike[L,W,R0], This <: PrefixTraversableOnce[K, V, This]](implicit bf:PrefixTreeLikeBuilder[L,W,R0]) {
    type Data    = ((K, This), X)
    type Context = Seq[Data]
    type Values  = (L, Option[W], L=>R)
    type R       = R0


    def mapValues(ctx:Context):Values
    def nextX(child:(K,This),ctx:Context):X

    def children(ctx:Context):TraversableOnce[(K,This)]      = ctx.head._1._2
    def childContext(child:((K,This),X),ctx:Context):Context = child +: ctx

    def mapChild(child:(K,This),ctx:Context):(L,R) = nextX(child, ctx) match {
      case null => null
      case x1   => childContext((child,x1),ctx) match {
        case null => null
        case d    => recur(d)
      }
    }

    final def childrenLoop[X](s:Context): (((L,R))=>X) => Unit = (g:((L,R))=>X) => for (z <- children(s)) {
      val r=mapChild(z, s)
      if (r!=null) g(r)
    }
    final def recur(ctx: Context):(L,R) = mapValues(ctx) match {
      case null => null
      case u    => buildResult(u,ctx)
    }


    def buildDefault(cur:Context,defa:K=>This,g:L=>K): L=>R = null
    def buildTreeElement(v:Values,b:PrefixTreeLikeBuilder[L,W,R]) = (v._1,b.result(v._2,v._3))

    def buildResult(v:Values,ctx:Context)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = {
      val loop = childrenLoop(ctx)
      val b    = bf.newEmpty
      loop { b += _ }
      buildTreeElement(v,b)
    }

    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): R = get(d0)._2

    @throws(classOf[NoSuchElementException])
    final def get(d0:Data): (L,R) = recur(Seq(d0)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }
}