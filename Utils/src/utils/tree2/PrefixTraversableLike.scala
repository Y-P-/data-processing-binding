package utils.tree2

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
trait PrefixTraversableLike[K, +V, +This <: PrefixTraversableLike[K, V, This]]
  extends TraversableLike[(K, This), This] { self:This =>
  //an alias that concrete classes can use as shortcut to refer to themselves
  protected[this] type Repr = This
  override def repr:Repr = self
  
  /** The value for the current node */
  def value: Option[V]
      
  /** The empty tree of the same type as this tree
   *  @return   an empty tree of type `This`.
   */
  def empty: Repr = newBuilder(None,Nil)
  
  /** A new instance of builder similar to the one used to build this tree element.
   *  It can be used to build elements of the same tree kind.
   */
  protected[this] def newBuilder:PrefixTraversableLikeBuilder[K,V,Repr]
  
  /** rebuilds this tree with a specific value */
  def withValue[W>:V,T>:Repr<:PrefixTraversableLike[K,W,T]](value:Option[W])(implicit bf:PrefixTraversableLikeBuilder[K,W,T]):T = bf.withValue(this, value)
  
  /** tells if the two following method should work with no exception */
  def isNavigable:Boolean = false
  /** the parent of that PrefixTree ; this is not required and usually makes the implementation heavier */
  def parent:Repr = throw new NotImplementedError(s"parent is not accessible in that implementation: $getClass")
  /** the depth within the tree ; this is not required and usually makes the implementation heavier */
  def depth:Int   = throw new NotImplementedError(s"depth is not accessible in that implementation: $getClass")
      
  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  def isNonSignificant = value.isEmpty && isEmpty 

  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable tree consisting only of those key where the key satisfies
   *          the predicate `p`. This results in a new tree in which keys that where
   *          removed now fall back on the default method.
   */
  def filterKeys(p: K => Boolean): Repr = filterAll(x => p(x._1))
  
  /** Filters this map by retaining only key/value satisfying a predicate.
   *  @param  p   the predicate used to test key/value
   *  @return an immutable tree consisting only of those items which satisfy
   *          the predicate `p`. This results in a new tree in which keys that
   *          were removed now fall back on the default method.
   */
  def filterAll(p: ((K,Repr)) => Boolean): Repr = {
    val bf = newBuilder
    if (!isEmpty) for (x:(K,Repr) <- this if p(x)) bf += ((x._1,x._2.filterAll(p)))
    bf.result(value)
  }
  
  def seq: Repr = this
  
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
    def recur(tt:T,cur:Repr):R = {
      val b=bf.newEmpty
      for (x:(K,This) <- cur)
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
  def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:O)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:Repr,oo:O):R = {
      val b=bf.newEmpty
      for (x:(K,This) <- cur)
        if (!strict || tt.isDefinedAt(x._1) && oo.isDefinedAt(x._1)) try { b += ((x._1, recur(tt(x._1),x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
      b.result(oo.value.flatMap(_(tt,cur)),null)
    }
    recur(t,this,op)
  }
  
  /** Similar to the previous method, but now we can build a tree of a different nature.
   *  Note that this operation is the most complex for trees and it allows extensive tree transformations.
   *  However, it procuces trees with no default : handling defaults for the produced tree would be rather
   *  convoluted, and in any case, defaults can be added afterwards if necessary by a simple map operation.
   *  @param t   the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree or op tree for 
   *                defined values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault or O.noDefault is an error or similar answer (e.g. empty)
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node
   *  @return the resulting transformed tree
   */
  def zipFull[U,L,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(K,T,Repr)=>(Option[L],Option[U]),O],R<:PrefixTreeLike[L,U,R]](k0:K,t:T,strict:Boolean,op:O)(implicit bf:PrefixTreeLikeBuilder[L,U,R]):R = {
    def recur(tt:T,cur:Repr,oo:O,u:Option[U]):R = {
      val b=bf.newEmpty
      for (x:(K,This) <- cur)
        if (!strict || tt.isDefinedAt(x._1) && oo.isDefinedAt(x._1)) {
          val (t1,o1) = try { (tt(x._1),oo(x._1)) } catch { case _:NoSuchElementException => (tt.empty,oo.empty) }
          o1.value match {
            case None    =>
            case Some(f) => val r = f(x._1,t1,x._2)
              r._1 match {
                case None    =>
                case Some(l) => b += ((l, recur(t1,x._2,o1,r._2)))
              }
          }
        }
      b.result(u,null)
    }
    val u0 = op.value match {
      case None    => None
      case Some(f) => f(k0,t,this)._2
    }
    recur(t,this,op,u0)
  }

  /** Transforms this tree by applying a function to every retrieved value.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f   the function used to transform the values of this tree.
   *  @return a tree which maps every element of this tree.
   *            The resulting tree is a new tree.
   */
  def map[W,T<:PrefixTraversableLike[K,W,T]](f:V=>W)(implicit bf:PrefixTraversableLikeBuilder[K,W,T]):T = {
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
    bf.result(value.map(f))
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
  def mapFull[L,W,T<:PrefixTraversableLike[L,W,T]](f:(K=>L,L=>K,V=>W))(implicit bf:PrefixTraversableLikeBuilder[L,W,T]):T = {
    if (!isEmpty) {
      val bf1 = bf.newEmpty
      bf ++= (for (x:(K,This) <- this) yield ((f._1(x._1),x._2.mapFull(f)(bf1))))
    }
    bf.result(value.map(f._3))
  }
  
  /** Transforms this tree by applying a function to every key.
   *  It works also on default values, but be aware that using deep trees
   *  in the default results may lead to a severe performance load.
   *  @param  f the function used to transform the keys of this tree.
   *  @return a tree which maps every element of this tree.
   *          The resulting tree is a new tree.
   */
  def mapKeys[L,W>:V,T<:PrefixTraversableLike[L,W,T]](f:(K=>L,L=>K))(implicit bf:PrefixTraversableLikeBuilder[L,W,T]):T = {
    if (!isEmpty) {
      val bf1 = bf.newEmpty
      bf ++= (for (x:(K,This) <- this) yield ((f._1(x._1),x._2.mapKeys[L,W,T](f)(bf1))))
    }
    bf.result(value)
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
  def flatMap[W,T<:PrefixTraversableLike[K,W,T]](f:V=>T)(implicit bf:PrefixTraversableLikeBuilder[K,W,T]):T = {
    val e = (if (value.isDefined) f(value.get) else bf.empty)
    bf ++= e
    if (!isEmpty) {
      val bf1 = bf.newEmpty
      bf ++= (for (x:(K,This) <- this) yield ((x._1,x._2.flatMap[W,T](f)(bf1))))
    }
    bf.result(e.value)
  }
  
  /** Creates a new tree obtained by updating this tree with a given key/value pair.
   *  @param    kv the key/value pair
   *  @tparam   L the type of the new keys
   *  @tparam   T the type of the added value
   *  @return   A new tree with the new key/value mapping added to this map.
   */
  def update1[W>:V,T>:Repr<:PrefixTraversableLike[K,W,T]](kv:(K,T))(implicit bf:PrefixTraversableLikeBuilder[K,W,T]): T
  
  /** Identical to the previous method, but more than one element is updated.
   */
  def update[W>:V,T>:Repr<:PrefixTraversableLike[K,W,T]](kv:(K,T)*)(implicit bf:PrefixTraversableLikeBuilder[K,W,T]): T = { val t=kv; update[W,T](t) }
  
  /** Identical to the previous method, but elements are passed through an iterable like rather than a built Seq.
   */
  def update[W>:V,T>:Repr<:PrefixTraversableLike[K,W,T]](kv:GenTraversableOnce[(K,T)])(implicit bf:PrefixTraversableLikeBuilder[K,W,T]): T =
    kv.foldLeft[T](repr)(_.update1[W,T](_))
    
  /** Adds a key/(value,tree) pair to this tree, returning a new tree.
   *  Enlarge subtrees without losing information is usualy the expected operation on trees.
   *  @param    kv the key/(value,tree) pair
   *  @tparam   T1 the type of the added tree
   *  @tparam   V1 the type of the added value
   *  @return   a new tree with the new binding added to this tree
   *
   *  @usecase  def + (kv: (K, (V,T)): T
   */
  def + [W>:V,T>:Repr<:PrefixTraversableLike[K,W,T]] (kv:(K,T))(implicit bf:PrefixTraversableLikeBuilder[K,W,T]): T = update[W,T](kv)

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
  def + [W>:V,T>:Repr<:PrefixTraversableLike[K,W,T]] (kv1:(K,T), kv2:(K,T), kvs:(K,T) *)(implicit bf:PrefixTraversableLikeBuilder[K,W,T]): T =
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
  def ++[W>:V, T1 >: Repr <: PrefixTraversableLike[K,W,T1]](xs: GenTraversableOnce[(K, T1)])(implicit bf:PrefixTraversableLikeBuilder[K,W,T1]): T1 =
    ((repr: T1) /: xs.seq) (_ + _)

  /* Overridden for efficiency. */
  override def toSeq: Seq[(K, Repr)] = toBuffer[(K, Repr)]
  override def toBuffer[C >: (K, Repr)]: ArrayBuffer[C] = {
    val result = new ArrayBuffer[C](size)
    copyToBuffer[C](result)
    result
  }
  /*
  /** Creates the canonical flat representation of the tree.
   *  Working with this supposes that your tree doesn't use degenerate branches (with no children and no value,
   *  i.e. with empty as a node somewhere in the tree)
   *  @return the canonical view of the tree
   */
  def seqView(topFirst:Boolean=true,trackDuplicate:Boolean=false) = new SeqView(topFirst,trackDuplicate)
  */
  /** This class is used to iterate deeply through the tree.
   *  @param cur the current sequence of key for the current element (from bottom to top!)
   *  @param topFirst is true if an element appears before its children, false otherwise
   *  @param seen is a set that is updated to track internal cross references ; if null, no such tracking happens (performance gain, but infinite loops possible)
   *//*
  protected class TreeForeach(cur:Seq[K],topFirst:Boolean,seen:scala.collection.mutable.Set[Repr]) extends TraversableLike[(K, TreeForeach), TreeForeach] {
    if (seen!=null) seen.add(self.repr)                        //remember that this tree is already being processed, to avoid loops in trees containing self-references 
    
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
  }*/
  /*
  /** The Tree can conveniently be viewed almost as a 'Map' with sequences of keys as key.
   *  This is very convenient to iterate through it.
   *  This also provides the best way to transform a Tree: it is much easier to transform the canonical form into
   *  a new canonical form and build a new tree from that.
   */
  protected class SeqView(topFirst:Boolean,trackDuplicate:Boolean) extends Iterable[(Seq[K], V)] {
    def iterator:Iterator[(Seq[K], V)] = new Iterator[(Seq[K], V)] {
      val i = new TreeForeach(Nil,topFirst,if (trackDuplicate) scala.collection.mutable.Set.empty else null)
      @tailrec def fetch:(Seq[K], Repr) = { if (!i.hasNext) null else { var x=i.next(); if (x._2.value.isDefined) x else fetch } }
      var cur = fetch
      def hasNext: Boolean = cur!=null
      def next(): (Seq[K], V) = { val c=cur; cur=fetch; (c._1.reverse,c._2.value.get) }
    }
    final def tree = self
    /** Transforms this seqView by applying a function to every retrieved value and key.
     *  @param  f the function used to transform the keys and values of this tree.
     *  @return a tree which maps every element of this tree.
     *          The resulting tree is a new tree. 
     */
    def flatMap[W](f:V=>GenTraversable[(GenTraversable[K],W)]):GenTraversable[(GenTraversable[K],W)] =
      for (x <- iterator.toTraversable; r <- f(x._2)) yield ((x._1 ++ r._1, r._2))
  }
  */
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
    super.addString(b, start, sep, end)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every tree is `"Map"`.
   */
  override def stringPrefix: String = value match {
    case None    => "Tree"
    case Some(v) => "Tree{"+v+"}"
  }

  override def toString = super[TraversableLike].toString
}

object PrefixTraversableLike {
  
  sealed class NavigableMode(val id:Int)
  val nonNavigable    = new NavigableMode(0)
  val unsafeNavigable = new NavigableMode(1)
  val safeNavigable   = new NavigableMode(2)
  
  /** The minimum for building the Params used by the Tree implementation.
   */
  class Params[K,+V,+T<:PrefixTraversableLike[K,V,T]] (
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
  abstract class Abstract[K, +V, +This <: PrefixTraversableLike[K, V, This]] extends PrefixTraversableLike[K, V, This] {
    this:This =>
  }
}

