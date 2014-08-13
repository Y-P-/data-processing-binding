package utils.tree2

import scala.collection.generic.Subtractable
import scala.collection.mutable.Builder
import scala.collection.IterableLike
import scala.collection.GenMapLike
import scala.collection.Parallelizable
import scala.collection.GenIterableLike
import scala.collection.AbstractSet
import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.collection.MapLike
import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap

trait PrefixTreeLike[K, +V, +This <: PrefixTreeLike[K, V, This]]
  extends PartialFunction[K, This]
     with IterableLike[(K, This), This]
     with Subtractable[K, This]
     with Equals{
self:This =>
  def value: Option[V]
  protected[this] type Repr = This
  override def repr:Repr = self

  /** The empty tree of the same type as this tree
   *   @return   an empty tree of type `This`.
   */
  def empty: Repr
  
  /** Removes a key from this tree, returning a new tree.
   *  @param    key the key to be removed
   *  @return   a new tree without a binding for `key`
   *
   *  @usecase  def - (key: A): Map[A, B]
   *    @inheritdoc
   */
  def - (key: K): Repr
  
  /** Adds a key/(value,tree) pair to this tree, returning a new tree.
   *  This standard operation uses the merge choice rather than the replace choice.
   *  Enlarge subtrees without losing information is usualy the expected operation on trees.
   *  @param    kv the key/(value,tree) pair
   *  @tparam   T1 the type of the added tree
   *  @tparam   V1 the type of the added value
   *  @return   a new tree with the new binding added to this tree
   *
   *  @usecase  def + (kv: (K, (V,T)): T
   */
  def + [L>:K,T>:Repr<:PrefixTreeLike[L,_,T]] (kv:(L,T)): T = update(kv,false)

  /** Creates a new iterator over all key/value pairs of this tree.
   *  This iterates only on the immediate children.
   *
   *  @return the new iterator
   */
  
  def iterator: Iterator[(K, Repr)]
  
  /** Creates a new iterator over all key/value pairs of this tree.
   *  This iterates only on all children and subchildren.
   *
   *  @return the new iterator
   */
  def deepIterator: Iterator[Repr] = new Iterator[Repr] {
    private var cur=self
    def hasNext: Boolean = cur!=null
    def next(): Repr = {
      val r=cur
      r
    }
  }
  
  /** A common implementation of `newBuilder` for all maps in terms of `empty`.
   *  Overridden for mutable maps in `mutable.MapLike`.
   *  Note that this newBuilder cannot build a mapped Tree (i.e. K, V, Repr different from original.)
   *  This limitation leads us to introduce a new kind of builder, which is more generic.
   */
  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr]
  
  /** Optionally returns the value associated with a key.
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
  def apply(key: K): Repr = get(key) match {
    case None => default(key)
    case Some(value) => value
  }

  /** Tests whether the tree is empty.
   *  @return `true` if the tree does not contain any key/value binding, `false` otherwise.
   */
  override def isEmpty: Boolean = size == 0

  /**  Returns the value associated with a key, or a default value if the key is not contained in the tree.
   *   @param   key      the key.
   *   @param   default  a computation that yields a default value in case no binding for `key` is
   *                     found in the tree.
   *   @tparam  D        the result type of the default computation.
   *   @return  the value associated with `key` if it exists,
   *            otherwise the result of the `default` computation.
   *
   *   @usecase def getOrElse(key: A, default: => B): B
   *     @inheritdoc
   */
  def getOrElse[T>:Repr](key: K, default: => T): T= get(key).getOrElse(default)


  /** Tests whether this tree contains a binding for a key.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def contains(key: K): Boolean = get(key).isDefined

  /** Tests whether this tree contains a binding for a key. This method,
   *  which implements an abstract method of trait `PartialFunction`,
   *  is equivalent to `contains`.
   *
   *  @param key the key
   *  @return    `true` if there is a binding for `key` in this tree, `false` otherwise.
   */
  def isDefinedAt(key: K) = contains(key)
  
  def seq: Repr = this

  /** Collects all keys of this tree in a set.
   * @return  a set containing all keys of this tree.
   */
  def keySet: Set[K] = new DefaultKeySet

  /** The implementation class of the set returned by `keySet`.
   */
  protected class DefaultKeySet extends AbstractSet[K] with Set[K] with Serializable {
    def contains(key : K) = self.contains(key)
    def iterator = keysIterator
    def + (elem: K): Set[K] = Set[K]() ++ this + elem
    def - (elem: K): Set[K] = Set[K]() ++ this - elem
    override def size = self.size
    override def foreach[C](f: K => C) = self.keysIterator foreach f
  }

  /** Creates an iterator for all keys.
   *
   *  @return an iterator over all keys.
   */
  def keysIterator: Iterator[K] = new AbstractIterator[K] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._1
  }

  /** The implementation class of the iterable returned by `values`.
   */
  protected class DefaultValuesIterable extends AbstractIterable[Repr] with Iterable[Repr] with Serializable {
    def iterator = valuesIterator
    override def size = self.size
    override def foreach[C](f: Repr => C) = self.valuesIterator foreach f
  }

  /** Creates an iterator for all values in this tree.
   *
   *  @return an iterator over all values that are associated with some key in this tree.
   */
  def valuesIterator: Iterator[Repr] = new AbstractIterator[Repr] {
    val iter = self.iterator
    def hasNext = iter.hasNext
    def next() = iter.next()._2
  }

  /** Defines the default value computation for the tree,
   *  returned when a key is not found
   *  The method implemented here throws an exception,
   *  but it might be overridden in subclasses.
   *
   *  @param key the given key value for which a binding is missing.
   *  @throws `NoSuchElementException`
   */
  def default(key: K): Repr =
    throw new NoSuchElementException("key not found: " + key)
  
  /** Filters this map by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original tree without copying any elements.
   */
  def filterKeys(p: K => Boolean): This

  /** Transforms this tree by applying a function to every retrieved value.
   *  @param  f   the function used to transform values of this tree.
   *  @return a tree view which maps every element of this tree
   *          to `f(this)`. The resulting tree is a new tree.
   */
  def map[C<:PrefixTreeLike[_,_,C]](f: Repr => C): C = f(this)  //yeah! that simple! but f is obviously recursive...
  
  /** Creates a new tree obtained by updating this tree with a given key/value pair.
   *  @param    kv the key/value pair
   *  @param    replace if true, the existing value is discarded, if false it is merged
   *  @tparam   L the type of the new keys
   *  @tparam   T the type of the added value
   *  @return   A new tree with the new key/value mapping added to this map.
   */
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](kv:(L,T),replace:Boolean): T
  
  /** Identical to the previous method, but more than one element is updated.
   */
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](replace:Boolean,kv:(L,T)*): T = update(replace,kv:_*)
  
  /** Identical to the previous method, but elements are passed through an iterable like rather than a built Seq.
   */
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](replace:Boolean,kv:IterableLike[(L,T),_]): T =
    kv.foldLeft[T](repr)(_.update(_,replace))

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
  def + [L>:K,T>:Repr<:PrefixTreeLike[L,_,T]] (kv1:(L,T), kv2:(L,T), kvs:(L,T) *): T =
    this + kv1 + kv2 ++ kvs

  /** Adds all key/value pairs in a traversable collection to this tree, returning a new map.
   *
   *  @param    xs  the collection containing the added key/value pairs
   *  @tparam   B1  the type of the added values
   *  @return   a new tree with the given bindings added to this tree
   *
   *  @usecase  def ++ (xs: Traversable[(A, B)]): Map[A, B]
   *    @inheritdoc
   */
  def ++[T1 >: Repr <: PrefixTreeLike[K,_,T1]](xs: GenTraversableOnce[(K, T1)]): T1 =
    ((repr: T1) /: xs.seq) (_ + _)

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
  override def toBuffer[C >: (K, Repr)]: scala.collection.mutable.Buffer[C] = {
    val result = new scala.collection.mutable.ArrayBuffer[C](size)
    copyToBuffer[C](result)
    result
  }
  
  def seqView = new SeqView
  
  /** This class is used to iterate deeply through the tree.
   *  @param cur the current sequence of key for the current element
   *  @param topFirst is true if an element appears before its children, false otherwise
   *  @param seen is a set that is updated to track internal cross references ; if null, no such tracking happens (performance gain, but infinite loops possible)
   */
  protected class TreeIterator(val cur:Seq[K],topFirst:Boolean,seen:scala.collection.mutable.Set[Repr]) extends AbstractIterator[(Seq[K], This)] {
    if (seen!=null) seen.add(repr)                             //remember that this tree is already being processed, to avoid loops in trees containing self-references 
    protected[this] val iter = iterator                        //iterator for this level
    protected[this] var i:Iterator[(Seq[K], This)] = getSub    //current sub-iterator
    protected[this] var done:Boolean = false                   //true when this item has been provided
    @tailrec final def hasNext:Boolean = !done || i!=null && (i.hasNext || {i=getSub; hasNext})
      //some clarifications: if this item has not been processed, there is a next
      //if there is no sub-iterator available and this item has been processed, we are finished
      //but if there is a sub-iterator with a next element, then there is a next
      //otherwise fetch the next sub-iterator (which could be null) and then check if it has a next element
    final def next(): (Seq[K], This) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,repr)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    final def getSub:Iterator[(Seq[K], This)] = {
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
   *  This is very convenient to build the Tree and iterate through it.
   *  This view provides such a map-like interface.
   */
  class SeqView extends PartialFunction[IterableLike[K,_], This] {
    def iterator(topFirst:Boolean,track:Boolean):Iterator[(Seq[K], Repr)] = new TreeIterator(Nil,topFirst,if (track) scala.collection.mutable.Set.empty else null)
    final def tree                                          = self
    def iterator: Iterator[(Seq[K], This)]                  = iterator(true,true)
    def apply(keys:IterableLike[K,_]):Repr                  = keys.foldLeft(self)(_(_))
    def isDefinedAt(keys: IterableLike[K,_]):Boolean        = get(keys)!=None
    def get(keys:IterableLike[K,_]):Option[Repr]            = keys.foldLeft[Option[This]](Some(self))((t,k)=>if (t==None) None else t.get.get(k))
    def apply(keys:K*):Repr                                 = apply(keys)
    def get(keys:K*):Option[Repr]                           = get(keys)
    /*
    def +[I<:IterableLike[K,I]](kv:(I,V)):This = {
      if (kv._1.isEmpty) self
      val (h,t)=(kv._1.head,kv._1.tail)    //head and tail
      self.get(h) match {                  //check current key in current tree
        case Some(s) => s                  //exists ? use found tree
        
      }
      if (t.isEmpty) (self + ((h,kv._2)))  //last key: update last tree with (key,value)
      else (self.get(h) match {            
        case Some(s) => s                  //exists ? use found tree
        case None    => self+((h,empty))   //doesn't exist ? insert empty tree for that key
      }).seqView + ((t,kv._2))             //update found subtree with tail
    }
    */
    //XXX
    def -(keys: Seq[K]): Repr                               = if (keys.length==1) (self - keys(0)) else (self(keys(0)).seqView - keys.tail)
  }
  object SeqView {
    implicit def toTree(s:SeqView) = s.tree    
  }

  /** Appends all bindings of this tree to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string
   *  `end`. Inside, the string representations of all bindings of this tree
   *  in the form of `key -> value` are separated by the string `sep`.
   *
   *  @param b     the builder to which strings are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    this.iterator.map { case (k, v) => k+" -> "+v }.addString(b, start, sep, end)

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
  implicit def toSeq[T<:PrefixTreeLike[_,_,T]](t:T):t.SeqView = t.seqView
}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

/** An abstract class for the trait. Used to share code.
 */
abstract class AbstractPrefixTree[K, +V, +This <: AbstractPrefixTree[K, V, This]] extends PrefixTreeLike[K, V, This] {
  this:This=>
}


/** A generic Builder for PrefixTreeLike which extends the standard Builder class.
 */
abstract class PrefixTreeLikeBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]](val empty: Tree) extends Builder[(K,Tree),Tree] {
  def apply(v:Option[V],tree:IterableLike[(K,Tree),_]):Tree
  final def apply(v:V,tree:(K,Tree)*):Tree = apply(Some(v),tree)
  final def apply(v:V,tree:IterableLike[(K,Tree),_]):Tree = apply(Some(v),tree)
  final def apply(v:V):Tree = apply(Some(v),empty)
  final def apply(tree:(K,Tree)*):Tree = apply(None,tree)
  protected var elems: Tree = empty
  def +=(x: (K, Tree)): this.type = { elems = elems + x; this }
  def clear() { elems = empty }
  def result: Tree = elems
  
  /** inner utility : develops one level of data by tearing out the first elt of all inner iterables. */
  protected def develop(data:Traversable[(Traversable[K],V)]) = {
    val h = LinkedHashMap.empty[K,(Option[V],List[(Traversable[K],V)])]
    for (x <- data; first=x._1.head; value=(x._1.tail,x._2)) h.put(first,(value._1.isEmpty,h.get(first)) match {
      case (false,None)    => (None,List(value))    //create entry: intermediate leaf, init first child
      case (true, None)    => (Some(value._2),Nil)  //create entry: final leaf, put value, no children
      case (false,Some(l)) => (l._1,value::l._2)    //update entry: intermediate leaf, keep current value, update children
      case (true, Some(l)) => (Some(value._2),l._2) //update entry: final leaf, put value, keep children
    })
    h //note that children lists are in reverse order
  }
  /** Develops data to build a 'Tree' beginning with the (name,cur) leaf, associated with data expanded as subtree */
  def fromCanonical(data:Traversable[(Traversable[K],V)]):Tree =
    empty ++ (for ((k,(v,l)) <- develop(data)) yield (k,apply(v,fromCanonical(l))))
}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/


/** The standard implementation sits on Maps.
 *  This opens up some opportunities by using Map operations.
 *  Other esoteric implementation could exist, but this one should satisfy most needs, especially
 *  when it can be subclassed while keeping the same subtype for operation return (such as + etc.)
 *  See the StringTree subclass.
 */
class PrefixTree[K,+V](val value:Option[V], val tree: Map[K,PrefixTree[K,V]]) extends AbstractPrefixTree[K,V,PrefixTree[K,V]] {
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](kv:(L,T),replace:Boolean): T = kv._2 match {
    case t:PrefixTree[K,V]       => newBuilder(value,tree+((kv._1, if (replace) t else tree.get(kv._1) match { case None=>t; case Some(t1)=>t.update(false,t1) } )))
    case t:PrefixTreeLike[K,_,T] => t.empty//XXX.newBuilder()
  }
  def -(key: K): Repr                    = newBuilder(value,tree-key)
  def get(key: K): Option[Repr]          = tree.get(key)
  def iterator:Iterator[(K, Repr)]       = tree.iterator
  override def size: Int                 = tree.size
  override def empty: Repr               = newBuilder(None,tree.empty)
  def filterKeys(p: K => Boolean): Repr  = newBuilder(value,tree.filterKeys(p))

  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr] = PrefixTree.builder[K,V]
  
  /** A full map operation that can tranform key, value and Tree type.
   */
  def map[W,L,T<:PrefixTree[L,W] with PrefixTreeLike[L,W,T]](f:V=>W, g:K=>L)(implicit builder:PrefixTreeLikeBuilder[L,W,T]):T = {
    def h(r:Repr):T = builder(r.value.map(f),r.tree.map(x=>(g(x._1),h(x._2))))
    h(this)
  }

  /** A more usual map operation that only tranforms value and Tree type.
   */
  def map[W,T<:PrefixTree[K,W] with PrefixTreeLike[K,W,T]](f:V=>W)(implicit builder:PrefixTreeLikeBuilder[K,W,T]):T = {
    def h(r:Repr):T = builder(r.value.map(f),r.tree.map(x=>(x._1,h(x._2))))
    h(this)
  }
  
  def filterAll(p: ((K,Repr)) => Boolean): Repr  = {
    def h(r:Repr):Repr = newBuilder(r.value,r.tree.filter(p).map(x=>(x._1,h(x._2))))
    h(this)
  }
}

object PrefixTree {
  implicit def builder[K,V] = new PrefixTreeLikeBuilder[K,V,PrefixTree[K,V]](new PrefixTree[K,V](None,Map.empty[K,PrefixTree[K,V]])) {
    def apply(v: Option[V], tree: IterableLike[(K,PrefixTree[K,V]), _]):PrefixTree[K,V] = new PrefixTree[K,V](v,empty.tree++tree)
  }
  def apply[K,V](v:Option[V],tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V] = builder(v,tree)
  def apply[K,V](v:V,tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V]         = apply(Some(v),tree)
  def apply[K,V](tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V]             = apply(None,tree)
  def apply[K,V](v:V):PrefixTree[K,V]                                     = apply(Some(v),Map.empty[K,PrefixTree[K,V]])
}


/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

/** A common use case with String as key type.
 */
class StringTree[+V](value: Option[V],override val tree: Map[String,StringTree[V]]) extends PrefixTree[String,V](value,tree) with PrefixTreeLike[String,V,StringTree[V]] {
  protected[this] override def newBuilder:PrefixTreeLikeBuilder[String,V,Repr] = StringTree.builder[V]
}

object StringTree {
  implicit def builder[V] = new PrefixTreeLikeBuilder[String,V,StringTree[V]](new StringTree[V](None,Map.empty[String,StringTree[V]])) {
    def apply(v: Option[V], tree: IterableLike[(String,StringTree[V]), _]):StringTree[V] = new StringTree[V](v,empty.tree++tree)
  }
  def apply[V](v:Option[V],tree:Map[String,StringTree[V]]):StringTree[V] = builder(v,tree)
  def apply[V](v:V,tree:Map[String,StringTree[V]]):StringTree[V]         = apply(Some(v),tree)
  def apply[V](tree:Map[String,StringTree[V]]):StringTree[V]             = apply(None,tree)
  def apply[V](v:V):StringTree[V]                                        = apply(Some(v),Map.empty[String,StringTree[V]])
}

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/

object X {
  val c1 = StringTree(1)
  val c2 = StringTree(2)
  val c3 = StringTree(3)
  val c11 = StringTree(4,Map("a"->c1,"b"->c2))
  val c12 = StringTree(5,Map("c"->c3))
  val c111 = StringTree(6,Map("d"->c11,"e"->c12))
    
  def f(x:StringTree[Int]):StringTree[String] = StringTree(x.value.map(_.toString+"x"),x.tree.mapValues(f))
  def g(x:(String,PrefixTree[String,_])):Boolean = x._1!="b"
  def h(x:StringTree[Int]):PrefixTree[Int,String] = PrefixTree(x.value.map(_.toString+"x"),x.tree.map(x=>(x._1(0)-'a',h(x._2))))
  
  def main(a:Array[String]):Unit = {
    println(c111)
    val r = c111.map[StringTree[String]](f)      //deep
    println(r)
    val s = c111.filterAll(g)                    //one level
    println(s)
    val t = c111.map[PrefixTree[Int,String]](h)  //full mapping to other tree
    println(t)
    val q = c111.map(_.toString+"x",_(0) - 'a')
    println(q)
    val u = c111.map[String,StringTree[String]]((_:Int).toString+"x")
    println(u)
    for (z<-u.iterator(true,false)) println(z)
    println(c111.get("d"))
    println(c111.seqView.get("d","a"))
    println(c111.seqView.get("d","a","c"))
    println(c111("d"))
    println(c111.seqView(Seq("d","a")))
    try { println(c111.seqView(Seq("d","a","c"))) } catch { case e:java.util.NoSuchElementException => println("not found")}
    println(StringTree.builder.fromCanonical(Seq(
      (Seq("x","y"),1),
      (Seq("x","y","z"),2),
      (Seq("x"),3),
      (Seq("x","v"),4),
      (Seq("z"),5),
      (Seq("z","a","b"),6),
      (Seq("z"),7)
    )))
  }
}
