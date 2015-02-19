package utils.tree

/** This object provides methods that are used to iterate through trees.
 *
 *  This algorithm is rather general, and many flavors can be derived from it.
 *  This generality is handled through the Context abstract type, which comes
 *  in four flavors:
 *  - (K,This) : the processing is basic, and the methods used can only access
 *               the current child key/value pair. This is usually the most
 *               efficient iteration process.
 *               the trait for this are:
 *               - `RecurNoData`
 *               - `Standalone`
 *  - Seq[(K,This)] : the processing is deep, and the methods used can access
 *               the current child key/value pair, but also that of any of its
 *               parents.
 *               the trait for this are:
 *               - `RecurNoData`
 *               - `Standalone`
 *  - ((K,This),X) : the processing is complex, and the methods used can access
 *               some additional, user provided, data of type X. This data may
 *               change for each node and is provided through a nextX method.
 *               the trait for this are:
 *               - `RecurWithData`
 *               - `Layered`
 *  - Seq[((K,This),X)] : the processing is both deep and complex. This is of
 *               course the least efficient processing.
 *               the trait for this are:
 *               - `RecurWithData`
 *               - `Layered`
 *
 *  There are also three kinds of iteration that are defined:
 *  - Simple iteration through the nodes ; it can be used either for side-effects
 *    (~ foreach) or building a single result (~ fold)
 *    the base trait for this is `Iterate`
 *  - Map-like operations where the input node children are either ignored or
 *    transformed to one output node ; the result is a tree that is smaller
 *    than the input tree : it is a 1 -> 0,1 transformation
 *    the trait for this is `RecurBuilder`
 *  - Flatmap-like operations where the input node result can be enriched with
 *    additional nodes. This may involves replacing/merging/ignoring the additional
 *    nodes, depending on the strategy used and presence of key conflicts :
 *    it is a 1 -> 0,n transformation and it can be quite expensive when node
 *    conflicts arise (same key), which may result in partial tree merges, depending
 *    on the strategyu chosen to handle conflicts.
 *    the trait for this is `ExpandRecurBuilder`
 *  When the iteration produces a tree like structure, two kinds are available:
 *  - Views, which only implement `PrefixTraversableOnce` ; they can be seen as
 *    lightweight trees as they don't hold the whole tree data but are usually
 *    backed by an original tree and appropriate transformations. The traits
 *    that produce Views contain View in their name.
 *  - Trees, which implement `PrefixTreeLike` which usually involves building
 *    a full data structure. The traits that produce Trees contain Tree in their name.
 *
 *  Filtering operation are inherent to the algorithm.
 *  Precisely, the following conditions always hold:
 *  - if `nextX` returns `null`, the child for which X was computed is ignored
 *  - if `mapValue` returns `null`, the node for which it was called is ignored
 */
object PrefixLoop {

  /** Top trait for defining the generic recursion framework on PrefixIterableOnce.
   *
   *  It lets do most recursive calls that apply to trees, including map-like or
   *  even flatMap-like transformations. The sheer number of possible combinations
   *  will not make it possible to give a variant of each possible uses.
   *
   *  It is configurable by mixing in the appropriate subtraits.
   *  Many methods are made final to ensure that only compatible traits are mixed together.
   *  Many traits are 'orthogonal', in that they each define a set of the required methods :
   *  in the end, a number of traits must be mixed together to get a working class.
   *
   *  Usually, if the provided traits are used (or the appropriate combination provided
   *  through an abstract class), then the user only has to concentrate on two methods:
   *  - `def mapValues(ctx:Context):Values` which is the transformation itself (and must
   *     call `recur` at some point.) ; it is allowed to return `null`, which means that
   *     nothing will get produced for that context.
   *  - `def nextX(child:(K,This),ctx:Context):X` which, when external data is required
   *    for the transformation, provides the next layer of data for a given child within
   *    a given context ; it is allowed to return `null`, which means that nothing will
   *    be produced for that child.
   *
   *  This is not a trait to ensure sharing the base algorithm.
   *
   *  @tparam K the key type for the tree we are working on
   *  @tparam V the value type for the tree we are working on
   *  @tparam This the type of the tree we are working on
   */
  protected abstract class Recur {
    //target tree parameter types
    type K
    type V
    type This <: PrefixTraversableOnce[K, V, This]

    type Data>:Null     //Some basic information pertaining to the current layer
    type Context>:Null  //usually either Data (we don't need parents info) or Seq[Data] (we need parents info)
    type Values>:Null   //some kind of data required to build the result
    type Result         //Result produced by the recursion : null value is possible but is reserved (skipped node)

    /** this builds the `values` from a given context; very context dependent */
    protected def mapValues(ctx:Context):Values

    /** Given some Values and a Context, builds the converted result and associated key.
     *  This method is the one that does the actual work. See `PrefixTreeLike.partition` for a simple example
     *  of actual use.
     * @param ctx the current context
     * @param u the intermediate values built for that algorithm
     * @param loop is function that is called for each child, with the result found for that child and its context.
     *             it must be called to process the current node children (otherwise they are ignored.)
     */
    protected def buildResult(ctx:Context,u:Values,loop:((Result,Context)=>Any) => Unit):Result

    //usually provided by a generic trait/subclass (`RecWithDataRB` etc)
    /** this is used to start the recursion from the top layer : it builds the initial context */
    protected def initialize(d0:Data):Context
    /** current element in the transformed tree */
    protected def getCurrent(ctx:Context):(K,This)
    /** given some Data (a child layer) and Context, builds the Context for that child */
    protected def childContext(child:Data,ctx:Context):Context
    /** the data info for the child in the given context */
    protected def nextData(child:(K,This),ctx:Context):Data

    /** Applies the recursion on all the tree layers below ctx
     *  Non arborescent trees may contain infinite loops ; these may be detected (at a hefty processing cost)
     *  by using the seen parameter (disable if it is 'null') ; in that case, only the first such node is actually
     *  processed : repeated nodes are just ignored. However, seen contains, when the call returns, the list of
     *  all such nodes and context that were left unprocessed.
     *  For a use case, see the 'PrefixTreeLike.copy' method.
     *  @param ctx, the context on which children we are looping
     *  @param seen, the set of nodes that have been processed ; used to prevent infinite loops in non arborescent trees
     *         it can be 'null', in which case no check is done : this may dramatically speed up the recursion
     *         at the end, it contains all processed contexts and associated Return : entries where the List[Context]
     *         part contain more than one element correspond to skipped contexts.
     *  @param reprocess, the method that gets called whenever a result becomes available for some node that could not
     *         be processed immediately (due to a looping reference to itself among its children) ; that method is handled
     *         the context for the waiting node, the expected result, and a method that may return the result for any
     *         already computed node (including itself).
     *  @return the Result for that ctx ; it may be unknown if it is being processed (current node refers to a parent)
     */
    final protected def recur(ctx: Context, seen:java.util.IdentityHashMap[This,(List[Context],Option[Result])], reprocess:(Result,Context,This=>Option[Result])=>Unit):Option[Result] = {
      def fetch:This=>Option[Result] = x => if (seen.containsKey(x)) seen.get(x)._2 else None
      val nd = getCurrent(ctx)._2
      if (seen.containsKey(nd)) { //don't process : fetch current result
        val l = seen.get(nd)
        if (l._2==None)
          seen.put(nd,(ctx +: l._1, l._2))
        else
          reprocess(l._2.get,ctx,fetch)
        l._2
      } else {
        seen.put(nd,(Nil,None))
        val r = mapValues(ctx) match {
          case null => null.asInstanceOf[Result]
          case u    => buildResult(ctx, u, (loop:((Result,Context)=>Any)) => {
                         for (child <- nd)
                           nextData(child, ctx) match {
                             case null =>
                             case d    => val ctx1 = childContext(d,ctx)
                                          val r=recur(ctx1,seen,reprocess)
                                          if (loop!=null && r!=null) loop(r.get,ctx1)
                           }
                         })
        }
        val r1 = Some(r)
        val waiting = seen.get(nd)._1
        seen.put(nd,(Nil, r1))
        for (c <- waiting) reprocess(r,c,fetch)
        r1
      }
  }
  /** The simpler version
   */
  final protected def recur(ctx: Context):Result = mapValues(ctx) match {
    case null => null.asInstanceOf[Result]
    case u    => buildResult(ctx, u, (loop:((Result,Context)=>Any)) => {
                 val node = getCurrent(ctx)._2
                 for (child <- node)
                   nextData(child, ctx) match {
                     case null =>
                     case d    => val ctx1 = childContext(d,ctx)
                                  val r=recur(ctx1)
                                  if (loop!=null && r!=null) loop(r,ctx1)
                   }
                 })
        }

    /** Applies the transformation to a canonical or degenerated tree.
     *  In the latter case, duplicate nodes are handled as if they were fully distinct.
     *  @param d0, the initial layer
     *  @return the Result of the transformation
     */
    @throws(classOf[NoSuchElementException])
    final def get(d0:Data): Result = recur(initialize(d0))

    /** Applies the transformation to any tree, even infinite.
     *  Duplicate nodes are handled as soon as possible by the reprocess function.
     *  @param d0, the initial layer
     *  @param reprocess, the function to handle nodes waiting for their result
     *  @return the Result of the transformation
     */
    @throws(classOf[NoSuchElementException])
    final def get(d0:Data, reprocess:(Result,Context,This=>Option[Result])=>Unit): Result = recur(initialize(d0),new java.util.IdentityHashMap[This,(List[Context],Option[Result])],reprocess).get
  }

  //traits for cases with (or without) specific X data
  trait RecurNoData extends Recur {
    type Data = (K,This)
    final protected def nextData(child:(K,This),ctx:Context) = child
  }
  trait RecurWithData extends Recur {
    type X  //user type
    type Data = ((K,This),X)
    /** Extracts the Data for the current node in the context */
    protected def getCurrentX(ctx:Context):X
    /** this builds the user data for a given context and child; very user dependent */
    protected def nextX(child:(K,This),ctx:Context):X
    final protected def nextData(child:(K,This),ctx:Context) = nextX(child, ctx) match {
      case null => null
      case x    => (child,x)
    }
  }

  //traits for accessing (or not) parents
  trait Standalone extends Recur {
    type Context = Data
    final protected def childContext(child:Data,ctx:Context):Context = child
    final protected def initialize(d0:Data):Context = d0
  }
  trait Layered extends Recur {
    type Context = Seq[Data]
    final protected def childContext(child:Data,ctx:Context):Context = child +: ctx
    final protected def initialize(d0:Data):Context = Seq(d0)
  }

  /** These classes define how the current element is retrieved: this depends on the presence of X and
   *  on the Context type. Together, they are the basis for recursions on trees. As abstract classes they
   *  let share most of the basic methods.
   */
  abstract class RecWithDataRB extends RecurWithData with Layered {
    final protected def getCurrent(ctx:Context):(K,This) = ctx.head._1
    final protected def getCurrentX(ctx:Context):X = ctx.head._2
  }
  abstract class RecNoDataRB extends RecurNoData with Layered {
    final protected def getCurrent(ctx:Context):(K,This) = ctx.head
  }
  abstract class BasicWithDataRB extends RecurWithData with Standalone {
    final protected def getCurrent(ctx:Context):(K,This) = ctx._1
    final protected def getCurrentX(ctx:Context):X = ctx._2
  }
  abstract class BasicNoDataRB extends RecurNoData with Standalone {
    final protected def getCurrent(ctx:Context):(K,This) = ctx
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///                     Useful in mapLike transformations                        ///
  ////////////////////////////////////////////////////////////////////////////////////
  /** Used to bind generic parameters to abstract types
   */
  trait Binder[X0,K0,V0,L0,W0,T0<:PrefixTraversableOnce[K0,V0,T0],R0<:PrefixTraversableOnce[L0,W0,R0]] extends RecurBuilder {
    type X = X0
    override type K = K0
    override type V = V0
    override type L = L0
    override type W = W0
    override type R = R0
    override type This = T0
  }

  /** Defines a tree result type
   * @tparam L the key
   * @tparam W the value
   * @tparam R the result tree type
   */
  trait RecurBuilder extends Recur {
    type L
    type W
    type R<:PrefixTraversableOnce[L,W,R]
    type Result=(L,R)
    /** fetches the W value for the current node result */
    protected def getValue(v:Values):Option[W]
    /** fetches the L key for the current node result.
     *  A proper transformation should never result in conflicting L keys.
     *  This can only be ensured by the user.
     */
    protected def getResultKey(v:Values,ctx:Context):L
    /** Running against the current tree */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): R = get(d0)._2
    /** Running against the current tree */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data,reprocess:(Result,Context,This=>Option[Result])=>Unit): R = get(d0,reprocess)._2
  }

  //traits for building View
  trait ViewRB extends RecurBuilder {
    type R = PrefixTraversableOnce.Abstract[L,W]
    protected def buildResult(ctx:Context,v:Values,loop:((Result,Context)=>Any) => Unit):Result =
      (getResultKey(v,ctx),new R(getValue(v)) { def foreach[X](g:Result=>X):Unit = loop((r,c)=>g(r)) })
  }
  //traits for building Tree
  trait TreeRB extends RecurBuilder {
    type R<:PrefixTreeLike[L,W,R]
    /** the builder for the result tree */
    protected def bf:PrefixTreeLikeBuilder[L,W,R]
    /** the method that provides the default function for a given Value. Often, it is built using the next two methods.*/
    protected def getDefault(v:Values):L=>R

    //methods that help in converting a source K=>This default to a L=>R default
    /** The method that creates the L=>R default function from a given K=>This default function. */
    protected def buildDefault(ctx:Context,default:K=>This): L=>R
    /** a method that builds the result for a given child ; it is an extract from the standard algorithm,
     *  adapted to deal with defaults (throws exception). */
    protected def stdDefault(ctx:Context,child:(K,This)):R = (nextData(child, ctx) match {
      case null => throw new PrefixTreeLike.NoDefault(child._1)
      case d    => recur(childContext(d,ctx))
    })._2

    protected def buildResult(ctx:Context,v:Values,loop:((Result,Context)=>Any) => Unit):Result = {
      val b = bf.newEmpty
      loop {(r,c)=> b += r }
      (getResultKey(v,ctx),b.result(getValue(v),getDefault(v)))
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///               Useful in transformations with tree expansion                  ///
  ////////////////////////////////////////////////////////////////////////////////////

  //we want to maximize code sharing
  abstract class ExpandRecurBuilder extends RecurBuilder with ValueLWDE {
    /** defines how to handle key conflict
     *  @param ctx is the parent context
     *  @param r1 is the 'local' child
     *  @param r2 is the 'additional' child
     */
    protected def merge(ctx:Context,r1:Result,r2:Result):R

    /** This actually merges the children for the given context ; indeed, when expanding with
     *  additional children, there is the possibility of conflict between keys, and this has to
     *  be solved in some way.
     */
    protected def doMerge(ctx:Context,additional:Iterable[Result],loop:((Result,Context)=>Any) => Unit)(g:Result=>Any):Unit = {
      val add  = additional.toBuffer
      var done = 0
      if (add.size>0) {
        loop { (r,c)=>
          g((r._1, add.indexWhere(y=>y!=null && y._1==r._1) match {
            case -1 => r._2
            case  i => val y=add(i); add(i)=null; done+=1; merge(ctx,r,y)
          }))
        }
        if (done!=add.size) add.filter(_!=null).map(g)
      } else {
        loop((r,c)=>g(r))
      }
    }
  }

  abstract class ExpandRecWithDataRB extends ExpandRecurBuilder with RecurWithData with Layered {
    final protected def getCurrent(ctx:Context):(K,This) = ctx.head._1
    final protected def getCurrentX(ctx:Context):X = ctx.head._2
  }
  abstract class ExpandRecNoDataRB extends ExpandRecurBuilder with RecurNoData with Layered {
    final protected def getCurrent(ctx:Context):(K,This) = ctx.head
  }
  abstract class ExpandBasicWithDataRB extends ExpandRecurBuilder with RecurWithData with Standalone {
    final protected def getCurrent(ctx:Context):(K,This) = ctx._1
    final protected def getCurrentX(ctx:Context):X = ctx._2
  }
  abstract class ExpandBasicNoDataRB extends ExpandRecurBuilder with RecurNoData with Standalone {
    final protected def getCurrent(ctx:Context):(K,This) = ctx
  }

  //trait for building a View
  trait ExpandViewRB extends ExpandRecurBuilder with ViewRB {
    override protected def buildResult(ctx:Context,v:Values,loop:((Result,Context)=>Any) => Unit):Result =
      (getResultKey(v,ctx),new R(getValue(v)) { def foreach[X](g:Result=>X):Unit = doMerge(ctx,getAdditionalChildren(v),loop)(g) })
  }
  //trait for building a Tree
  trait ExpandTreeRB extends ExpandRecurBuilder with TreeRB {
    override protected def buildResult(ctx:Context,v:Values,loop:((Result,Context)=>Any) => Unit):Result = {
      val b = bf.newEmpty
      doMerge(ctx,getAdditionalChildren(v),loop){ b += _ }
      val r = (getResultKey(v,ctx),b.result(getValue(v),getDefault(v)))
      r
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///                      Additional traits to plug holes                         ///
  ////////////////////////////////////////////////////////////////////////////////////
  /** Handling defaults when building trees: we want to provide a way to fall back on the original
   *  tree defaults. If the original is only PrefixTraversableOnce, overriding getDefault will be
   *  more useful. Falling back on the original default requires to be able to navigate back
   *  from an L to an appropriate K key ; this is particularly easy when K <-> L is bijective.
   */
  trait SameKeyDefault extends TreeRB {
    override type L=K
    final protected def buildDefault(ctx:Context,defa:K=>This): L=>R = l => stdDefault(ctx,(l,defa(l)))
  }
  trait OtherKeyDefault extends TreeRB {
    def reverseKey: L=>K
    final protected def buildDefault(ctx:Context,defa:K=>This): L=>R = if (reverseKey==null) null else l => {
      val k = reverseKey(l)
      stdDefault(ctx,(k,defa(k)))
    }
  }
  trait WithThisTreeDefault extends TreeRB {
    type This <: PrefixTreeLike[K,V,This]
    /** The method that creates the L=>R default function from the source default function if it exists. */
    protected def buildStdDefault(ctx:Context): L=>R = buildDefault(ctx,getCurrent(ctx)._2.default)
  }

  /** These traits define common uses for the Values type.
   */
  trait ValueW extends RecurBuilder {
    override type L=K
    type Values = Option[W]
    final protected def getValue(v:Values):Option[W] = v
    final protected def getResultKey(v:Values,ctx:Context):L = getCurrent(ctx)._1
  }
  trait ValueWD extends RecurBuilder {
    override type L=K
    type Values = (Option[W], L=>R)
    final protected def getValue(v:Values):Option[W] = v._1
    final protected def getDefault(v:Values):L=>R = v._2
    final protected def getResultKey(v:Values,ctx:Context):L = getCurrent(ctx)._1
  }
  trait ValueLW extends RecurBuilder {
    type Values = (L, Option[W])
    final protected def getValue(v:Values):Option[W] = v._2
    final protected def getResultKey(v:Values,ctx:Context):L = v._1
  }
  trait ValueLWD extends RecurBuilder {
    type Values = (L, Option[W], L=>R)
    final protected def getValue(v:Values):Option[W] = v._2
    final protected def getDefault(v:Values):L=>R = v._3
    final protected def getResultKey(v:Values,ctx:Context):L = v._1
  }
  trait ValueLWDE extends RecurBuilder {
    type Values = (L, Option[W], L=>R, Iterable[(L,R)])
    final protected def getValue(v:Values):Option[W] = v._2
    final protected def getDefault(v:Values):L=>R = v._3
    final protected def getResultKey(v:Values,ctx:Context):L = v._1
    final protected def getAdditionalChildren(v:Values):Iterable[(L,R)] = v._4
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///       Useful in iterative transformations (deepForeach, fold...)             ///
  ////////////////////////////////////////////////////////////////////////////////////

  trait Iterate[X0,R0,K0,V0,T0<:PrefixTraversableOnce[K0,V0,T0]] extends Recur {
    type X=X0
    type Result=R0
    type K=K0
    type V=V0
    type This=T0
    //that's it mostly: Values is nothing specific. But it cannot be Null (ignored), nor Unit (must be >:Null) ; so Context is fine.
    type Values = Context
    //redefine buildResult in a more pratictal way: having now twice the same context as parameter is a bit redundant
    protected def deepLoop(ctx:Context,loop:((Result,Context)=>Any) => Unit):Result
    //fixes mapValues
    final protected def mapValues(ctx:Context):Values = ctx
    //fixes buildResult
    def buildResult(ctx:Context,v:Values,loop:((Result,Context)=>Any) => Unit):Result = deepLoop(ctx,loop)
    /** Running against the current tree. */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data): Result = get(d0)
    /** Running against the current tree. */
    @throws(classOf[NoSuchElementException])
    final def apply(d0:Data,reprocess:(Result,Context,This=>Option[Result])=>Unit): Result = get(d0,reprocess)
  }

  /** Defines some common behavior when the iteration deals with applying a method to each node.
   *  Actually the most common use!
   */
  trait FuncIterate[K,V,O,T<:PrefixTraversableOnce[K,V,T]] extends Iterate[O,Unit,K,V,T] {
    protected[this] def f:(Context,=>Unit)=>Any
    protected def deepLoop(ctx:Context,loop:((Result,Context)=>Any)=>Unit):Result = f(ctx,loop(null))
  }
  /** Adds in the case where we are zipping with a companion tree.
   *  Here, the companion tree contains a type F, which the user can transform into an appropriate function.
   */
  protected[this] trait ZipFuncIterate[K,V,F,O<:PrefixTreeLike[K,F,O],T<:PrefixTraversableOnce[K,V,T]] extends FuncIterate[K,V,O,T] { this:RecurWithData =>
    //this is used to convert a more user friendly function F into the required function which operates on Context
    protected[this] val h:F=>(Context,=>Unit)=>Unit
    val f:(Context,=>Unit)=>Unit = (ctx,recur)=>getCurrentX(ctx).value match {
      case None    => recur
      case Some(g) => if (g!=null) h(g)(ctx,recur)
    }
    protected def nextX(child:(K,This), ctx:Context):O = try { getCurrentX(ctx)(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }
  }

  /** builds a deep iterator that unwinds the operation tree in parallel with the input tree.
   * @param h0 a method to transform F (a user-provided function) into the function of two arguments that is expected:
   *          - the Context that is built while unwinding the trees
   *          - =>Unit, the Recursion call that is built while unwinding the trees
   *          And which returns Unit
   *          It is assumed that:
   *          - if a node in O contains None, then the recursion proceeds, but (obviously), the current node is skipped
   *          - if a node in O contains Some(null), then the current node and its children are skipped
   *          - otherwise, the recursion proceeds normally and the matching function is applied on the current node
   * @tparam K the key type
   * @tparam V the value type
   * @tparam This the tree type
   * @tparam F the type provided by the user ; usually a function
   * @tparam O the tree type for F
   *
   * @return a deep tree iterator that applies F on the nodes of the tree
   */
  def zipRec[K,V,F,O<:PrefixTreeLike[K,F,O],This<:PrefixTraversableOnce[K,V,This]](h0:F=>(Seq[((K,This),O)],=>Unit)=>Unit) = new RecWithDataRB with ZipFuncIterate[K,V,F,O,This] {
    val h=h0
  }
  def zip[K,V,F,O<:PrefixTreeLike[K,F,O],This<:PrefixTraversableOnce[K,V,This]](h0:F=>(((K,This),O),=>Unit)=>Unit) = new BasicWithDataRB with ZipFuncIterate[K,V,F,O,This] {
    val h=h0
  }
  /** builds a deep iterator.
   * @param h0 the function of two arguments that is expected:
   *          - the Context that is built while unwinding the trees
   *          - =>Unit, the Recursion call that is built while unwinding the trees
   *          And which returns Unit
   *          That function will be called on each node.
   * @tparam K the key type
   * @tparam V the value type
   * @tparam This the tree type
   * @tparam O the tree type for F
   *
   * @return a deep tree iterator that applies F on the nodes of the tree
   */
  def loopRec[K,V,This<:PrefixTraversableOnce[K,V,This]](h0:(Seq[(K,This)],=>Unit)=>Any) = new RecNoDataRB with FuncIterate[K,V,Nothing,This] {
    val f=h0
  }
  def loop[K,V,This<:PrefixTraversableOnce[K,V,This]](h0:((K,This),=>Unit)=>Any) = new BasicNoDataRB with FuncIterate[K,V,Nothing,This] {
    val f=h0
  }

  //Useful for fold operations, where we have to update a 'global' result at each traversed node
  def fold[U,Context](u0:U,topFirst:Boolean,f: (U, Context) => U)(g:((Context, =>Unit) => Unit) => Unit):U = {
    var u = u0
    g(if (topFirst) (ctx,recur) => { u=f(u,ctx); recur } else (ctx,recur) => { recur; u=f(u,ctx) })
    u
  }
  //id above, but with an extractor Context => P involved
  def fold[U,Context,P](u0:U,topFirst:Boolean,extract:Context=>P)(g:(((U, P) => U) => ((Context, => Unit) => Unit)) => Unit) = {
    var u = u0
    g(x => if (x==null) null else if (topFirst) (ctx,recur) => { u=x(u,extract(ctx)); recur } else (ctx,recur) => { recur; u=x(u,extract(ctx)) })
    u
  }
  //convenient for the rather usual case where the user manipulates a (P , => Unit) => Any function to work with,
  //rather than the less user-friendly (Context,=>Unit)=>Unit one.
  def extractor[Context,P](extract:Context=>P):(((P , => Unit) => Any))=>(Context,=>Unit)=>Unit = g => (ctx,recur) => extract(ctx) match {
    case null =>
    case x    => g(x,recur)
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ///                    Classes for mapLike transformations                       ///
  ////////////////////////////////////////////////////////////////////////////////////

  /** Actual classes for all possible combinations.
   *  - with or without an X user data
   *  - with or without access to the parents
   *  - using same key as input or transforming the key
   *  - building Views (PrefixTraversableOnce) or Trees (PrefixTreeLike)
   */
  abstract class RecurRecView[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends RecWithDataRB with ViewRB with ValueLW with Binder[X,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class RecurRecTree[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R])  extends RecWithDataRB with OtherKeyDefault with ValueLWD with Binder[X,K,V,L,W,T,R]
  abstract class RecurRecViewNoData[K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends RecNoDataRB with ViewRB with ValueLW with Binder[Nothing,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class RecurRecTreeNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends RecNoDataRB with OtherKeyDefault with ValueLWD with Binder[Nothing,K,V,L,W,T,R]
  abstract class RecurView[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends BasicWithDataRB with ViewRB with ValueLW with Binder[X,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class RecurTree[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends BasicWithDataRB with OtherKeyDefault with ValueLWD with Binder[X,K,V,L,W,T,R]
  abstract class RecurViewNoData[K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends BasicNoDataRB with ViewRB with ValueLW with Binder[Nothing,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class RecurTreeNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R])  extends BasicNoDataRB with OtherKeyDefault with ValueLWD with Binder[Nothing,K,V,L,W,T,R]
  abstract class RecurRecViewSameKey[X,K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends RecWithDataRB with ViewRB with ValueW with Binder[X,K,V,K,W,T,PrefixTraversableOnce.Abstract[K,W]]
  abstract class RecurRecTreeSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R])  extends RecWithDataRB with SameKeyDefault with ValueWD with Binder[X,K,V,K,W,T,R]
  abstract class RecurRecViewNoDataSameKey[K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends RecNoDataRB with ViewRB with ValueW with Binder[Nothing,K,V,K,W,T,PrefixTraversableOnce.Abstract[K,W]]
  abstract class RecurRecTreeNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R]) extends RecNoDataRB with SameKeyDefault with ValueWD with Binder[Nothing,K,V,K,W,T,R]
  abstract class RecurViewSameKey[X,K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends BasicWithDataRB with ViewRB with ValueW with Binder[X,K,V,K,W,T,PrefixTraversableOnce.Abstract[K,W]]
  abstract class RecurTreeSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R]) extends BasicWithDataRB with SameKeyDefault with ValueWD with Binder[X,K,V,K,W,T,R]
  abstract class RecurViewNoDataSameKey[K,V,W,T<:PrefixTraversableOnce[K,V,T]] extends BasicNoDataRB with ViewRB with ValueW with Binder[Nothing,K,V,K,W,T,PrefixTraversableOnce.Abstract[K,W]]
  abstract class RecurTreeNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],T<:PrefixTraversableOnce[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[K,W,R])  extends BasicNoDataRB with SameKeyDefault with ValueWD with Binder[Nothing,K,V,K,W,T,R]

  abstract class ExpandRecView[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ExpandRecWithDataRB with ExpandViewRB with Binder[X,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class ExpandRecTree[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTreeLike[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends ExpandRecWithDataRB with ExpandTreeRB with Binder[X,K,V,L,W,T,R]
  abstract class ExpandRecViewNoData[K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ExpandRecNoDataRB with ExpandViewRB with Binder[Nothing,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class ExpandRecTreeNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTreeLike[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends ExpandRecNoDataRB with ExpandTreeRB with Binder[Nothing,K,V,L,W,T,R]
  abstract class ExpandView[X,K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ExpandBasicWithDataRB with ExpandViewRB with Binder[X,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class ExpandTree[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTreeLike[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends ExpandBasicWithDataRB with ExpandTreeRB with Binder[X,K,V,L,W,T,R]
  abstract class ExpandViewNoData[K,V,L,W,T<:PrefixTraversableOnce[K,V,T]] extends ExpandBasicNoDataRB with ExpandViewRB with Binder[Nothing,K,V,L,W,T,PrefixTraversableOnce.Abstract[L,W]]
  abstract class ExpandTreeNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],T<:PrefixTreeLike[K,V,T]](implicit val bf:PrefixTreeLikeBuilder[L,W,R]) extends ExpandBasicNoDataRB with ExpandTreeRB with Binder[Nothing,K,V,L,W,T,R]
}