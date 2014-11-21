package utils.tree

import scala.collection.TraversableLike
import scala.collection.AbstractSet
import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.Subtractable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import scala.annotation.tailrec
import scala.runtime.AbstractPartialFunction
import java.util.NoSuchElementException
import scala.collection.IterableLike
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

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
 */
trait PrefixTraversableOnce[K, +V, +This <: PrefixTraversableOnce[K, V, This]]
  extends TraversableOnce[(K, This)] { self:This =>
  //an alias that concrete classes can use as shortcut to refer to themselves
  protected[this] type Repr = This
  type Key = K
  type Value <: V
  
  /** The value for the current node */
  def value: Option[V]
      
  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  def isNonSignificant = value.isEmpty && isEmpty 
  
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
  def zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:(T,Repr)=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur: =>Repr):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (!strict || tt.isDefinedAt(x._1)) try { b += ((x._1, recur(tt(x._1),x._2))) } catch { case _:NoSuchElementException => }
      b.result(op(tt,cur),null)
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
  def zip2[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:PrefixTreeLike[K,(T,Repr)=>Option[U],_])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type O = PrefixTreeLike[K,(T,Repr)=>Option[U],_]  //see zipFull
    def recur(tt:T,cur: =>Repr,oo:O):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (strict.succeeds(tt,oo)(x._1)) try { b += ((x._1, recur(tt(x._1),x._2,oo(x._1).asInstanceOf[O]))) } catch { case _:NoSuchElementException => }
      b.result(oo.value.flatMap(_(tt,cur)),null)
    }
    recur(t,this,op)
  }

  /** Similar to the previous method, but now we can build a tree of a fully different nature (different keys.)
   *  Note that this operation is the most complex for trees and it allows extensive tree transformations.
   *  @param t   the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree or op tree for 
   *                defined values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault or O.noDefault is an error or similar answer (e.g. empty)
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node.
   *             an operator produces the new key (if None, the node is skipped), new value, new default handler (can of course be null.)
   *  @return the resulting transformed tree
   */
  def zipFull[L,U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[L,U,R]](t:T,op:PrefixTreeLike[K,(K,T,Repr)=>(Option[L],Option[U],L=>R),_],k0:K,default:L=>R,strict:Strictness)(implicit bf:PrefixTreeLikeBuilder[L,U,R]):R = {
    //we should declare a generic O<:PrefixTreeLike[K,(K,T,Repr)=>(Option[L],Option[U],L=>R),O]
    //but doing so prevents generic type inferrence
    //we cannot declare a recursive type, hence _ is inferred as Any, and not as <:PrefixTreeLike[K,(K,T,Repr)=>(Option[L],Option[U],L=>R),_]
    //so we will safely cast to O (because we know the return of apply to be of the same type)
    type O = PrefixTreeLike[K,(K,T,Repr)=>(Option[L],Option[U],L=>R),_]
    def recur(tt:T,cur:Repr,oo:O,u:Option[U],default:L=>R):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (strict.succeeds(tt,oo)(x._1)) {
          var t1:T = null.asInstanceOf[T]
          var o1:O = null
          var ok = true
          //a missing elt (NoSuchElementException) returned by default will not produce any result
          try {
            t1 = tt(x._1)
            o1 = oo(x._1).asInstanceOf[O]
          } catch { case _:NoSuchElementException => ok=false }
          if (ok) o1.value match {
              case Some(f)  =>
                val r = f(x._1,t1,x._2)
                r._1 match {
                  case Some(l) => b += ((l, recur(t1,x._2,o1,r._2,r._3)))
                  case _ =>
                }
              case _ =>
            }
        }
      b.result(u,default)
    }
    val u0 = op.value match {
      case None    => None
      case Some(f) => f(k0,t,this)._2
    }
    recur(t,this,op,u0,default)
  }
    
  /** A fold left operation that descends through the subtrees.
   *  Children are evaluated before their parent.
   */
  def deepFoldLeft[U](u0:U,k0:K)(f: (U, (K,Repr)) => U): U = {
    def recur(u: U, t:(K,Repr)):U = f(t._2.foldLeft(u)(recur),t)
    recur(u0,(k0,this))
  }
  /** As above.
   *  When processing children, access to the parent is handled down.
   *  @param u0 the initial value
   *  @param k0 a key for the top element
   *  @param f the method used for folding ; it takes three parameters
   *           U the current value
   *           Seq[(K,Repr)] the element stack from bottom to top
   */
  def deepFoldLeft1[U](u0:U,k0:K)(f: (U, Seq[(K,Repr)]) => U): U = {
    def recur(u: U, elt:Seq[(K,Repr)]):U = f(elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt)),elt)
    recur(u0,Seq((k0,this)))
  }
  /** As above.
   *  The operation can change as it is provided through a tree.
   *  Branches for which an operation is missing (NoSuchElementException) are not explored.
   *  Nodes with no value are ignored (but children are explored.)
   */
  def deepFoldLeft2[U](u0:U,k0:K)(op: PrefixTreeLike[K,(U, Seq[(K,Repr)]) => U,_]): U = {
    type O = PrefixTreeLike[K,(U, Seq[(K,Repr)]) => U,_]  //see zipFull
    def recur(u: U, elt:Seq[(K,Repr)], oo:O):U = if (oo==null) u else {
      val u1 = elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt,try { oo(ee._1).asInstanceOf[O] } catch { case _:NoSuchElementException => null }))
      oo.value match {
        case Some(g) => g(u1,elt) //has value: compute current element
        case None    => u1        //no value: do nothing
      }
    }
    recur(u0,Seq((k0,this)),op)
  }
    
  /** A recursive call that descends through the subtrees.
   *  Children are all evaluated within the context of their parent, i.e.
   *  within the 'op' call on their parent.
   *  This produces no result.
   *  @param k an initial key for the top node
   *  @param op, the operation to execute on each node.
   *             (K,Repr) : the current element and its key
   *             =>Unit   : a byname param that has to be evaluated
   *                        somewhere to evaluate the current element children
   */
  def deepForeach(k:K)(op: ((K,Repr),=>Unit) => Unit): Unit = {
    def recur(elt:(K,Repr)):Unit = op(elt,elt._2.foreach(recur))
    recur((k,this))
  }
  
  /** As above.
   *  Evaluating children produces results that can be used by the parent.
   *  Furthermore, children can be evaluated but don't have to be.
   *  Evaluating children is done by iterating on the provided iterator.
   *  @param k an initial key for the top node ; it is hardly used
   *  @param op, the operation to execute on each node.
   *             (K,Repr)    : the current element and its key
   *             Iterator[U] : the iterator on the children
   *             U           : some result
   */
  def deepForeach1[U](k:K)(op: ((K,Repr),Iterator[U]) => U): U = {
    def recur(elt:(K,Repr)):U = op(elt,elt._2.toIterator.map(recur))
    recur((k,this))
  }
  
  /** As above.
   *  Children can reach their parent (but not above.)
   *  @param k0 an initial key for the top node ; it is hardly used
   *  @param op, the operation to execute on each node.
   *             Seq[(K,Repr)] : the elements sequence, starting from the current element
   *             Iterator[U]   : the iterator on the children
   *             U             : some result
   */
  def deepForeach2[U](k0:K)(op: (Seq[(K,Repr)],Iterator[U]) => U): U = {
    def recur(elt:Seq[(K,Repr)]):U = op(elt,elt.head._2.toIterator.map(x=>recur(x+:elt)))
    recur(Seq((k0,this)))
  }
  
  /** As above.
   *  Children can reach their parents.
   *  @param k an initial key for the top node ; it is hardly used
   *  @param op, the operation to execute on each node.
   *             this is a tree and the actual operation can change on each key.
   *             all required key entries must have a matching op.
   *             Repr     : the parent
   *             (K,Repr) : the current element and its key
   *             Iterator : the iterator on the children
   */
  def deepForeach3(k0:K)(op: PrefixTreeLike[K,(Seq[(K,Repr)],Iterator[Unit]) => Unit,_]): Unit = {
    type O = PrefixTreeLike[K,(Seq[(K,Repr)],Iterator[Unit]) => Unit,_]  //see zipFull
    def recur(elt:Seq[(K,Repr)], oo:O):Unit = if (oo!=null && oo.value!=None) {
      oo.value.get(elt,elt.head._2.toIterator.map(x=>recur(x+:elt,try { oo(x._1).asInstanceOf[O] } catch { case _:NoSuchElementException => null }))) //has value: compute current element
    }
    recur(Seq((k0,this)),op)
  }  

}

object PrefixTraversableOnce {
  import utils.BlockingData
    
  /** This interface is sufficient to create a PrefixTraversableOnce
   *  Whenever you have a new key, you push it.
   *  Whenever you have a value for a key, you pull it.
   *  Whenever you reach the end of a layer, you pull out.
   *  e.g. for this xml: <a><b>1</b>2<c>3</c></a>
   *     push(a)
   *     push(b)
   *     pull(1)
   *     pull
   *     pull(2)
   *     push(c)
   *     pull(3)
   *     pull
   *     pull
   */
  trait PushPull[-K,-V] {
    def push(key:K):Unit
    def pull(value:V):Unit
    def pull():Unit
  }
  
  /** marker for the PushPull */
  val End = new AnyRef
  
  /** Used to turn the push/pull into a PrefixTraversableOnce.
   *  The 'once' is not to trifle with: even 'hasNext' cannot be called more than once per element!
   *  This implementation is naive as it doesn't care about handling any error.
   *  In particular, the sending thread will lock if a second value is sent to the same item.
   *  XXX improve the naive implementation
   *  
   * @param K, the key type ; it is not allowed to be an Option, nor to be null.
   * @param V, the value type
   * @param item, the blocking buffer
   */
  class Layer[K,V](item:BlockingData[AnyRef]) extends Iterator[(K,Layer[K,V])] with PrefixTraversableOnce[K,V,Layer[K,V]] {
    var next:(K,Layer[K,V])  = _
    var value:Option[V] = None
    @tailrec final def hasNext:Boolean = item.take match {
        case `End`       => false
        case o:Option[V] => if (value.isDefined) throw new IllegalStateException("the same element cannot receive two values")
                            value = o
                            hasNext
        case k:K         => next = (k,new Layer[K,V](item))
                            true
      }    
  }
  
  /** Complement to the previous class to send the push/pull commands.
   * @param K, the key type ; it is not allowed to be an Option, nor to be null
   * @param V, the value type
   */
  class PullAdapter[K<:AnyRef,V] extends PushPull[K,V] {
  
    protected val item = new BlockingData[AnyRef]
    
    /** sends the appropriate command to the imbedded Layer */
    final def push(k:K) = item.put(k)
    final def pull(v:V) = item.put(Some(v))
    final def pull      = item.put(End)
  
    /** converts the push/pull sequence through the given operator. */
    def run[U](op: ((K,Layer[K,V]),Iterator[U]) => U)(implicit executionContext:ExecutionContext):Future[U] =
      Future { (new Layer[K,V](item)).deepForeach1[U](null.asInstanceOf[K])(op) }
  }
  
  /** Creates a pair for creating a PrefixTraversableOnce using a Push/Pull interface */
  def apply[U,K<:AnyRef,V](op: ((K,Layer[K,V]),Iterator[U]) => U)(implicit executionContext:ExecutionContext):(PushPull[K,V],Future[U]) = {
    val p = new PullAdapter[K,V]
    (p, p.run(op))
  }
}

