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
import PrefixTraversableOnce._

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
trait PrefixTraversableOnce[K, +V, +This]
  extends TraversableOnce[(K, This with PrefixTraversableOnce[K, V, This])] { self:PrefixTraversableOnce[K, V, This] with This =>
  //internal type used to lighten up the code
  private[this] type This0 = This with PrefixTraversableOnce[K, V, This]
  //internal type that may later be used to refer to this type
  protected[this] type Repr = This
  
  /** The value for the current node */
  def value: Option[V]
      
  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  def isNonSignificant = value.isEmpty && isEmpty
  
  def withActor[U>:V,R,P<:PrefixActor[(K,This0),U,P,R]](pa:P with PrefixActor[(K,This0),U,P,R]):R = {
    def recur(cur:This0,p:P):R = {
      for (x <- cur)   { val r  = p.onInit(x)
        if (r!=null)   { val rr = recur(x._2,r)
          if (rr!=null)  p += (x,rr)
        }
      }
      p.onEnd(cur.value)
    }
    recur(this,pa)
  }
  
  def withActorRec[R](k0:K,pa:PrefixActorRec[(K,This),R,_]):R = {
    pa.recur((k0,this))
  }
  
  
  def asTree0[U>:V,R<:PrefixTreeLike[K,U,R]](implicit bf:PrefixTreeLikeBuilder[K,U,R]) = {
    withActor(new Builder)
  }
  def asTree1[U>:V,R<:PrefixTreeLike[K,U,R]](implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    null.asInstanceOf[R]
    //new BuilderRec[K,U,R,This].recur((null.asInstanceOf[K],this))
  }
  
  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   */
  def asTree[U>:V,R<:PrefixTreeLike[K,U,R],O<:PrefixTreeLike[K,K=>R,O]](default:O with PrefixTreeLike[K,K=>R,O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(cur: This0,oo:O):R = {
      val b=bf.newEmpty
      for (x <- cur)
        try { b += ((x._1, recur(x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
      b.result(cur.value,oo.value.orNull)
    }
    recur(this,default)
  }  
  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   */
  def asTree[U>:V,R<:PrefixTreeLike[K,U,R]](implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(cur: This0):R = {
      val b=bf.newEmpty
      for (x <- cur) b += ((x._1, recur(x._2)))
      b.result(cur.value,null)
    }
    recur(this)
  }
  
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
  def zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean)(op:(T,This)=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:This0):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (!strict || tt.isDefinedAt(x._1)) try { b += ((x._1, recur(tt(x._1),x._2))) } catch { case _:NoSuchElementException => }
      b.result(op(tt,cur),null)
    }
    recur(t,this)
  }
  /** Same, but creates a view.
   */
  def zipView[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean)(op:(T,This)=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(tt:T,cur:This0):PrefixTraversableOnce.Abstract[K,U] = new PrefixTraversableOnce.Abstract[K,U](op(tt,cur)) {
      def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = for (x <- cur)
        if (!strict || tt.isDefinedAt(x._1)) try { f((x._1, recur(tt(x._1),x._2))) } catch { case _:NoSuchElementException => }
    }
    recur(t,this)
  }
  /** Same, but creates a view.
   *  Access to all parents of the current element is possible.
   */
  def zipViewRec[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean)(op:Seq[(T,This)]=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(s:Seq[(T,This0)]):PrefixTraversableOnce.Abstract[K,U] = {
      new PrefixTraversableOnce.Abstract[K,U](op(s)) {
        def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = {
          val cur = s.head
          for (x <- cur._2)
            if (!strict || cur._1.isDefinedAt(x._1)) try {
              val s1 = (cur._1(x._1),x._2) +: s
              f((x._1, recur(s1)))
            } catch { case _:NoSuchElementException => }
        }
      }
    }
    recur(Seq((t,this)))
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
  def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,This)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,This)=>Option[U],O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:This0,oo:O):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (strict.succeeds(tt,oo)(x._1)) try { b += ((x._1, recur(tt(x._1),x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
      b.result(oo.value.flatMap(_(tt,cur)),null)
    }
    recur(t,this,op)
  }
  /** Same, but creates a view.
   */
  def zip2View[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,This)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,This)=>Option[U],O]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(tt:T,cur:This0,oo:O):PrefixTraversableOnce.Abstract[K,U] = new PrefixTraversableOnce.Abstract[K,U](oo.value.flatMap(_(tt,cur))) {
      def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = for (x <- cur)
        if (strict.succeeds(tt,oo)(x._1)) try { f((x._1, recur(tt(x._1),x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
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
  def zipFull[L,U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[(K,This,T)])=>(Option[L],Option[U],L=>R),O],R<:PrefixTreeLike[L,U,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[(K,This,T)])=>(Option[L],Option[U],L=>R),O])(implicit bf:PrefixTreeLikeBuilder[L,U,R]):R = {
    def recur(elt:Seq[(K,This0,T)],oo:O):(L,R) = {
      val b=bf.newEmpty
      for (x <- elt.head._2)
        if (strict.succeeds(elt.head._3,oo)(x._1)) {
          var t1:T = null.asInstanceOf[T]
          var o1:O = null.asInstanceOf[O]
          if(try {
            //a missing elt (NoSuchElementException) will not produce any result
            t1 = elt.head._3(x._1)
            o1 = oo(x._1)
            true
          } catch { case _:NoSuchElementException => false }) {
            val res = recur((x._1,x._2,t1)+:elt,o1)
            if (res!=null) b += res
          }
        }
      oo.value match {
        case None    => null
        case Some(f) =>
          val res = f(elt)
          res._1 match {
            case Some(l) => (l,b.result(res._2,res._3))
            case _       => null
          }
      }
    }
    recur(Seq((k0,this,t)),op)._2
  }
  /** Same, but creates a view. The default value method goes away in this case.
   */
  def zipFullView[L,U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[(K,This,T)])=>(Option[L],Option[U]),O],R<:PrefixTreeLike[L,U,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[(K,This,T)])=>(Option[L],Option[U]),O]):PrefixTraversableOnce[L,U,PrefixTraversableOnce.Abstract[L,U]] = {
    def recur(elt:Seq[(K,This0,T)],oo:O):(L,PrefixTraversableOnce.Abstract[L,U]) =
      oo.value match {
        case None    => null
        case Some(f) =>
          val res = f(elt)
          res._1 match {
            case Some(l) => (l,new PrefixTraversableOnce.Abstract[L,U](res._2) {
              def foreach[X](g:((L,PrefixTraversableOnce.Abstract[L,U]))=>X):Unit = for (x <- elt.head._2)
                if (strict.succeeds(elt.head._3,oo)(x._1)) {
                  var t1:T = null.asInstanceOf[T]
                  var o1:O = null.asInstanceOf[O]
                  if(try {
                    //a missing elt (NoSuchElementException) will not produce any result
                    t1 = elt.head._3(x._1)
                    o1 = oo(x._1)
                    true
                  } catch { case _:NoSuchElementException => false }) {
                    val res = recur((x._1,x._2,t1)+:elt,o1)
                    if (res!=null) g(res)
                  }
                }
            })
            case _ => null
          }
      }
    recur(Seq((k0,this,t)),op)._2
  }
  
  /** A fold left operation that descends through the subtrees.
   *  Children are evaluated before their parent.
   */
  def deepFoldLeft[U](u0:U,k0:K)(f: (U, (K,This)) => U): U = {
    def recur(u: U, t:(K,This0)):U = f(t._2.foldLeft(u)(recur),t)
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
  def deepFoldLeft1[U](u0:U,k0:K)(f: (U, Seq[(K,This)]) => U): U = {
    def recur(u: U, elt:Seq[(K,This0)]):U = f(elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt)),elt)
    recur(u0,Seq((k0,this)))
  }
  /** As above.
   *  The operation can change as it is provided through a tree.
   *  Branches for which an operation is missing (NoSuchElementException) are not explored.
   *  Nodes with no value are ignored (but children are explored.)
   */
  def deepFoldLeft2[U,O<:PrefixTreeLike[K,(U, Seq[(K,This)]) => U,O]](u0:U,k0:K)(op: O with PrefixTreeLike[K,(U, Seq[(K,This)]) => U,O]): U = {
    def recur(u: U, elt:Seq[(K,This0)], oo:O):U = if (oo==null) u else {
      val u1 = elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt,try { oo(ee._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }))
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
  def deepForeach(k:K)(op: ((K,This),=>Unit) => Unit): Unit = {
    def recur(elt:(K,This0)):Unit = op(elt,elt._2.foreach(recur))
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
  def deepForeach1[U](k:K)(op: ((K,This),Iterator[U]) => U): U = {
    def recur(elt:(K,This0)):U = op(elt,elt._2.toIterator.map(recur))
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
  def deepForeach2[U](k0:K)(op: (Seq[(K,This)],Iterator[U]) => U): U = {
    def recur(elt:Seq[(K,This0)]):U = op(elt,elt.head._2.toIterator.map(x=>recur(x+:elt)))
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
  def deepForeach3[O<:PrefixTreeLike[K,(Seq[(K,This)],Iterator[Unit]) => Unit,O]](k0:K)(op: O with PrefixTreeLike[K,(Seq[(K,This)],Iterator[Unit]) => Unit,O]): Unit = {
    def recur(elt:Seq[(K,This0)], oo:O):Unit = if (oo!=null && oo.value!=None) {
      oo.value.get(elt,elt.head._2.toIterator.map(x=>recur(x+:elt,try { oo(x._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }))) //has value: compute current element
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
  class Layer[K,V](item:BlockingData[AnyRef]) extends Iterator[(K,Layer[K,V] with PrefixTraversableOnce[K,V,Layer[K,V]])] with PrefixTraversableOnce[K,V,Layer[K,V]] {
    var next:(K,Layer[K,V] with PrefixTraversableOnce[K,V,Layer[K,V]])  = _
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
  
  abstract class Abstract[K,V](val value:Option[V]) extends Traversable[(K,Abstract[K,V])] with PrefixTraversableOnce[K,V,Abstract[K,V]] {
    this:Abstract[K,V] =>
  }
  
  trait PrefixActor[-X,-V,+P,R] {
    def onInit(x:X):P
    def += (x:X,result:R):Unit
    def onEnd(v:Option[V]):R
  }
  
  trait PrefixActorRec[-X,+R,+P] { this:P=>
    protected[this] type P0=P
    
    protected[this] def i(x:X):Iterable[X]
    protected[this] def onInit(s:Seq[(X,P)],x:X):P0
    protected[this] def += (s:Seq[(X,P)],result:R):Unit
    protected[this] def onEnd(s:Seq[(X,P)]):R
    
    protected[this] def recur(s:Seq[(X,P)]):R = {
      val p = s.head._2
      i(s.head._1).foreach { x=>
        val r = onInit(s,x)
        if (r!=null) {
          val s1 = (x,r)+:s; val rr=recur(s1)
          if (rr!=null) this += (s1, rr)
        }      
      }
      onEnd(s)
    }
    def recur(x:X):R = recur(Seq((x,this)))
  }
  
  class Builder[K,V,R<:PrefixTreeLike[K,V,R]](implicit bf:PrefixTreeLikeBuilder[K,V,R]) extends PrefixActor[(K,Any),V,Builder[K,V,R],R] {
    val b=bf.newEmpty
    def onInit(x:(K,Any)):Builder[K,V,R] = new Builder[K,V,R]
    def += (x:(K,Any),result:R):Unit     = b += ((x._1,result))
    def onEnd(value:Option[V]):R         = b.result(value,null)
  }
  class BuilderRec[K,+V,+R<:PrefixTreeLike[K,V,R],R1<:PrefixTraversableOnce[K,V,R1]](implicit bf:PrefixTreeLikeBuilder[K,V,R]) extends PrefixActorRec[(K,R1),R,BuilderRec[K,V,R,R1]] {
    protected[this] type S = Seq[((K,R1),BuilderRec[K,V,R,R1])]
    protected[this] val b=bf.newEmpty
    protected[this] def i(x:(K,R1)):Iterable[(K,R1)] = x._2.toIterable
    protected[this] def onInit(s:S,x:(K,R1)):P0 = new BuilderRec[K,V,R,R1]
    protected[this] def +=(s:S,result:R):Unit   = b += ((s.head._1._1,result))
    protected[this] def onEnd(s:S):R            = b.result(s.head._1._2.value,null)
  }
  
  abstract class UU[K,-V,R] extends scala.collection.Seq[(K,UU[K,V,R])] with PushPull[K,V] with PrefixActorRec[K,R,UU[K,V,R]] {
    protected[this] var v:Option[V] = None
    /*
    def push(key:K):Unit   = onInit(s,key)
    def pull(value:V):Unit = v = Some(value)
    def pull():Unit        = s.head._2.+=(s,onEnd(s))
    
    protected[this] def i(x:K):Iterable[K] = null
    protected[this] def onInit(s:S,x:K):P0 = new UU((x,this)+:s)
    protected       def += (s:S,result:R):Unit
    protected[this] def onEnd(s:S):R    */
 //   def rcv() = head._2.+=(this,onEnd(this))
    
    def iterator: Iterator[(K, UU[K,V,R])] = ???
    def apply(idx: Int): (K, UU[K,V,R]) = ???
    def length: Int = ???     
  }  

}

