package utils.tree

import scala.collection.AbstractIterator
import scala.collection.AbstractIterable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.annotation.tailrec

/** Describes a tree precursor where data is reached through an iterable (key,sub-tree).
 *  As such, this structure presents a numerous number of interesting properties, the least of which
 *  being that this representation can also be used as views to actual PrefixTree implementations.
 *  Thus, when some complex operation on a tree is undertaken, and one doesn't want to build intermediate
 *  trees, lifting a real Tree to this representation will gain some benefits.
 *
 *  Methods are provided to go both ways between PrefixTreeLike and PrefixTraversableOnce.
 *
 *  For more information about PrefixTreeLike, se that class description which contains additional information.
 *
 *  Methods in this class often come by three:
 *
 *  method        => does something and return an actual Tree structure (usually through an implicit builder)
 *                   it is best used when one requires an actual tree as result
 *  methodView    => as method, but returns a view, i.e. PrefixTraversableOnce
 *                   it is best used when the result is going to be passed to some other tree transforming method
 *  methodViewRec => as mehodView, but in addition the functions passed as parameter can reach up to the top
 *                   of the (key,subtree) recursion, getting information about the parents
 *                   it is obviously more expensive and complex as the other methods and should only be used
 *                   when necessary. No version returning an actual tree is provided as this is not thought to
 *                   bring any significant gain.
 *
 *  These methods often take a transforming function as parameter.
 *  When this function returns a Tuple or an Option, then as a general contract, it is also allowed to return
 *  null: this indicates that the element (and subtree) is not to be processed and this can serve as an imbedded
 *  filter method : using filter is very often useless (and indeed more costly) when used in conjunction with
 *  such methods.
 *
 *  Methods:
 *  - filter remove elements (and their subtree) from the result
 *  - map transforms a tree into another tree using some transformation way
 *  - zip transform a tree into another tree by using at least one other tree ran in parallel
 *  - deepForeach runs through the tree elements in depth, in the tree order, but beginning at the bottom
 *    hence, the last elemen processed is usually the top tree itself
 *  - deepFoldLeft is similar to foldleft in other collections, but it runs through all elements in the
 *    same way as described in deepForeach
 *  Note that only a handful of the most usual operations are provided ; specific transformations are
 *  easy to achieve by using the relevant generic PrefixTraversableOnce.apply method with appropriate
 *  parameters. There are just too many possible uses.
 *
 *  In many cases, the transformation methods can themselves be provided as trees, which are also ran through
 *  in parallel with the processed tree. This paves the way to some very complex tree transformations.
 *
 *  Also note that for regularity, most methods require that you pass a "virtual" key for the top tree (which
 *  by definition is not assumed to be a sub-tree and not attached with a key); such a key may or may not be
 *  significant depending on your own provided transformation methods : the methods here have no use with it.
 */
trait PrefixTraversableOnce[K, +V, +This <: PrefixTraversableOnce[K, V, This]]
  extends TraversableOnce[(K, This)] { self:This =>
  //aliases that concrete classes can use as shortcut to refer to themselves or their component
  protected[this] type Repr = This
  type Key = K
  type Value <: V

  /** The value for the current node */
  def value: Option[V]

  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  def isNonSignificant = value.isEmpty && isEmpty

  protected override def reversed = super.reversed

  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   *  This is rather generic as it allows specific defaults for each node.
   */
  def toTree[U>:V,R<:PrefixTreeLike[K,U,R],O<:PrefixTreeLike[K,K=>R,O]](default:O with PrefixTreeLike[K,K=>R,O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),O)
    val values = (s:Data)                => (s._1._2.value,try { if (s._2==null) null else s._2(s._1._1).value.orNull } catch { case _:NoSuchElementException => null })
    val next   = (child:(K,This),s:Data) => s._2(child._1)
    toTreeSameKey[O,U,R](null.asInstanceOf[K],default,values,next)
  }

  /** Forces this PrefixTraversableOnce into a default PrefixTree.
   *  The method differ on how they generate the default: nodefault, constant default, default provided through a tree
   */
  def toPrefixTree:PrefixTree[K,V] = toTreeSameKey[V,PrefixTree[K,V]](null.asInstanceOf[K],x=>(x._2.value,null))
  def toPrefixTree[U>:V](default:K=>PrefixTree[K,U]):PrefixTree[K,U] = toTreeSameKey[U,PrefixTree[K,U]](null.asInstanceOf[K],x=>(x._2.value,default))
  def toPrefixTree[U>:V,O<:PrefixTreeLike[K,K=>PrefixTree[K,U],O]](default:O with PrefixTreeLike[K,K=>PrefixTree[K,U],O]):PrefixTree[K,U] = toTree[U,PrefixTree[K,U],O](default)

  /** filter this tree according to some criteria.
   *  When returning a PrefixTreeLike, `default` must be provided and cannot be null (but can return null if you want no default)
   */
  def filter[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(f:((K,This))=> Boolean, default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R =
    toTreeSameKey[U,R](k0,(x:(K,This))=>{if (f(x)) (x._2.value,default(x)) else null})
  def filterRec[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(f:(Seq[(K,This)])=>Boolean, default:(Seq[(K,This)])=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]) =
    toTreeRecSameKey[U,R](k0,(x:Seq[(K,This)])=>{if (f(x)) (x.head._2.value,default(x)) else null})
  def filterView(k0:K)(f:((K,This))=> Boolean):PrefixTraversableOnce.Abstract[K, V] =
    transformSameKey(k0,(x:(K,This))=>{if (f(x)) x._2.value else null})
  def filterViewRec(k0:K)(f:(Seq[(K,This)])=>Boolean):PrefixTraversableOnce.Abstract[K, V] =
    transformRecSameKey(k0,(x:Seq[(K,This)])=>{if (f(x)) x.head._2.value else null})

  /** map on this tree.
   *  Default are handled as above where necessary.
   */
  def map[U,R<:PrefixTreeLike[K,U,R]](k0:K)(f:V=>U,default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R =
    toTreeSameKey(k0,(x:(K,This))=>(x._2.value.flatMap(z=>Some(f(z))),default(x)))
  def mapRec[L,U,R<:PrefixTreeLike[L,U,R]](k0:K)(f:(Seq[(K,This)])=>(L,Option[U],L=>R))(implicit bf:PrefixTreeLikeBuilder[L,U,R]):R =
    toTreeRec(k0,(x:Seq[(K,This)])=>f(x))
  def mapView[U](k0:K)(f:V=>U):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] =
    transformSameKey(k0,(x:(K,This))=>x._2.value.flatMap(z=>Some(f(z))))
  def mapViewRec[L,U](k0:K)(f:(Seq[(K,This)])=>(L,Option[U])):PrefixTraversableOnce[L,U,PrefixTraversableOnce.Abstract[L,U]] =
    transformRec(k0,(x:Seq[(K,This)])=>f(x))

  /** Transform this tree with another tree.
   *  Both trees are explored 'in parallel' and each sub-node of this tree is transformed using the
   *  corresponding sub-node of the transformer tree using the provided `op`. The resulting tree is
   *  a subtree of the original tree (as far as keys are concerned) in that the operation only provides
   *  result values (no result nodes), and some nodes may be skipped (so less nodes is possible) when `op`
   *  returns null, the second tree contains a `null` for the current node, or the default from the second
   *  second tree exits with a NoSuchElementException.
   *  When the result is a PrefixLikeTree, it contains a default matching the input default.
   *  @param t      the transformer tree ; a node value that is null will skip the corresponding input node.
   *  @param strict if `true`, default values for the second tree are not used and nodes for which the key
   *                doesn't match are ignored. If `false`, a `null` result or NoSuchElementException
   *                error will also ignore the corresponding input node.
   *  @param op     an operator that transforms a T node with the matching current node to give the
   *                expected Option[U] value of the result node. If it instead returns null, the current
   *                node is skipped.
   *  @return the resulting transformed tree
   */
  def zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:(T,This)=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),T)
    val values = (cur:Data)              => { val r=op(cur._2,cur._1._2); if (r==null) null else (r, null) }
    val next   = (child:(K,This),s:Data) => if (!strict || s._2.isDefinedAt(child._1)) try { s._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    toTreeSameKey(null.asInstanceOf[K],t,values,next)
  }
  def zipRec[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:Seq[((K,This),T)]=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = Seq[((K,This),T)]
    val values = (cur:Data)              => { val r=op(cur); if (r==null) null else (r, null) }
    val next   = (child:(K,This),s:Data) => if (!strict || s.head._2.isDefinedAt(child._1)) try { s.head._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    toTreeRecSameKey(null.asInstanceOf[K],t,values,next)
  }
  def zipView[U,T<:PrefixTreeLike[K,_,T]](t:T,strict:Boolean,op:(T,This)=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    type Data = ((K,This),T)
    val values = (cur:Data)              => op(cur._2,cur._1._2)
    val next   = (child:(K,This),s:Data) => if (!strict || s._2.isDefinedAt(child._1)) try { s._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    transformSameKey(null.asInstanceOf[K],t,values,next)
  }
  def zipViewRec[U,T<:PrefixTreeLike[K,_,T]](t:T,strict:Boolean,op:Seq[((K,This),T)]=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    type Data = Seq[((K,This),T)]
    val values = (cur:Data)              => op(cur)
    val next   = (child:(K,This),s:Data) => if (!strict || s.head._2.isDefinedAt(child._1)) try { s.head._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    transformRecSameKey(null.asInstanceOf[K],t,values,next)
  }

  /** Similar to the previous method, but the operation is provided through a third tree which is explored
   *  'in parallel' too so that it can change at each input node.
   *  Input nodes are skipped when:
   *  - the transformer tree corresponding node is `null` or doesn't exist in strict mode. If not strict,
   *    it may still be skipped if the default returns null or throws a NoSuchElementException.
   *  - or the same conditions for the operation tree
   *  - or the operation applied on the source and transformer tree yields `null`.
   *  @param t   the transformer tree
   *  @param strict acts similarly to the `zip` behaviour, but it can act on none, one or both T and O trees.
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node
   *  @return the resulting transformed tree
   */
  def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,This)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,This)=>Option[U],O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),(T,O))
    val values = (cur:Data)              => { val r=cur._2._2.value.flatMap(_(cur._2._1,cur._1._2)); if (r==null) null else (r,null) }
    val next   = (child:(K,This),s:Data) => strict(child._1,s._2._1,s._2._2)
    toTreeSameKey(null.asInstanceOf[K],(t,op),values,next)
  }
  def zip2View[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,This)=>Option[U],O]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,This)=>Option[U],O]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    type Data = ((K,This),(T,O))
    val values = (cur:Data)              => cur._2._2.value.flatMap(_(cur._2._1,cur._1._2))
    val next   = (child:(K,This),s:Data) => strict(child._1,s._2._1,s._2._2)
    transformSameKey(null.asInstanceOf[K],(t,op),values,next)
  }
  def zip2ViewRec[W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,Seq[((K,This),(T,O))]=>Option[W],O]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,Seq[((K,This),(T,O))]=>Option[W],O]):PrefixTraversableOnce[K,W,PrefixTraversableOnce.Abstract[K,W]] = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data)              => cur.head._2._2.value.flatMap(_(cur))
    val next   = (child:(K,This),s:Data) => strict(child._1,s.head._2._1,s.head._2._2)
    transformRecSameKey(null.asInstanceOf[K],(t,op),values,next)
  }

  /** Similar to the previous method, but now we can build a tree of a fully different nature (different keys.)
   *  The conditions for skipping nodes are the same as above.
   *  @param t   the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree or op tree for
   *                defined values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault or O.noDefault is an error or similar answer (e.g. empty)
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node.
   *             an operator produces the new key (if None, the node is skipped), new value, new default handler (can of course be null.)
   *  @return the resulting transformed tree
   */
  def zipFullRec[L,W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W],L=>R),O],R<:PrefixTreeLike[L,W,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W],L=>R),O])(implicit bf:PrefixTreeLikeBuilder[L,W,R]):R = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data)              => cur.head._2._2.value.map(_(cur)).orNull
    val next   = (child:(K,This),s:Data) => strict(child._1,s.head._2._1,s.head._2._2)
    toTreeRec(k0,(t,op),values,next)
  }
  def zipFullRecView[L,W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W]),O],R<:PrefixTreeLike[L,W,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,Seq[((K,This),(T,O))]=>(L,Option[W]),O]):PrefixTraversableOnce[L,W,_] = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data)              => cur.head._2._2.value.map(_(cur)).orNull
    val next   = (child:(K,This),s:Data) => strict(child._1,s.head._2._1,s.head._2._2)
    transformRec(k0,(t,op),values,next)
  }

  /** Iterates through all the nodes of the tree. For each node, this method invokes f for its side-effect.
   *  The iteration order for the children of a given node depends on the underlying implementation for
   *  retrieving said children. Top nodes get invoked before children nodes. However, f is passed a parameter
   *  that is used to invoke the children : so depending where this invocation is done within f, it is
   *  possible to get the side effect of children before or after the parent's one.
   *  @param k0 an initial key for the top node (f requires it, but it may be irrelevant to your computation)
   *  @param f, the operation to execute on each node. It has the following parameters:
   *            (K,This) : the current node and its key
   *            =>Unit   : a by-name parameter that should be evaluated somewhere to evaluate the current node
   *                       children ; as a result, children invocation may also be skipped if you fail to invoke
   *                       this parameter.
   */
  def deepForeach(k0:K)(f:((K,This),=>Unit)=>Any):Unit = PrefixLoop.loop[K,V,This](f)((k0,this))
  /** Same as above, except that f can reach all the way to the top node within the call stack.
   *  In the Seq[(K,This)], head refers to the current (lowest) node.
   */
  def deepForeachRec(k0:K)(f:(Seq[(K,This)],=>Unit)=>Any):Unit = PrefixLoop.loopRec[K,V,This](f)((k0,this))
  /** Same as above, except that f is provided through a companion tree and can possibly change for each node.
   *  For a given key, three possible cases:
   *  - null or non existent (NoSuchElementException) matching operation: ignore that node and children
   *  - node with None value: don't compute that node, but go on with the children
   *  - node with a significant value: process normally
   */
  def deepForeachZip[O<:PrefixTreeLike[K,((K,This),=>Unit)=>Any,O]](k0:K)(op:O with PrefixTreeLike[K,((K,This),=>Unit)=>Any,O]):Unit =
    PrefixLoop.zip[K,V,((K,This),=>Unit)=>Any,O,This](PrefixLoop.extractor(_._1))(((k0,this),op))
  /** Same as above with the function being able to reach all the way to the top node
   */
  def deepForeachZipRec[O<:PrefixTreeLike[K,(Seq[(K,This)],=>Unit)=>Any,O]](k0:K)(op:O with PrefixTreeLike[K,(Seq[(K,This)],=>Unit)=>Any,O]):Unit =
    PrefixLoop.zipRec[K,V,(Seq[(K,This)],=>Unit)=>Any,O,This](PrefixLoop.extractor(_.view.map(_._1)))(((k0,this),op))


  /** Fold left operations that descends through the subtrees.
   *  Children are evaluated before their parent if topFirst is false.
   *  The top tree is evaluated with k0 as key.
   *  @param u0 the initial value
   *  @param k0 the key associated with the top tree
   *  @param topFirst if true, the children are evaluated after the parent
   *  @param f the function that is applied on each node
   *  @tparam U this type for the fold operation
   */
  def deepFoldLeft[U](u0:U,k0:K,topFirst:Boolean)(f: (U, (K,This)) => U): U =
    PrefixLoop.fold(u0,topFirst,f)(deepForeach(k0) _)
  /** Same as above, but f can reach the current node parents
   */
  def deepFoldLeftRec[U](u0:U,k0:K,topFirst:Boolean)(f: (U, Seq[(K,This)]) => U): U =
    PrefixLoop.fold(u0,topFirst,f)(deepForeachRec(k0) _)
  /** Same as above, but the function is provided through a companion tree.
   *  See `deepForeachRec` for filtering behavior
   */
  def deepFoldZip[U,O<:PrefixTreeLike[K,(U,(K,This))=>U,O]](u0:U,k0:K,topFirst:Boolean)(op:O with PrefixTreeLike[K,(U,(K,This))=>U,O]) =
    PrefixLoop.fold(u0,topFirst,(_:((K,This),O))._1)(PrefixLoop.zip[K,V,(U,(K,This))=>U,O,This](_)(((k0,this),op)))
  /** Same as above with the function being able to reach all the way to the top node
   */
  def deepFoldZipRec[U,O<:PrefixTreeLike[K,(U,Seq[(K,This)])=>U,O]](u0:U,k0:K,topFirst:Boolean)(op:O with PrefixTreeLike[K,(U,Seq[(K,This)])=>U,O]) =
    PrefixLoop.fold(u0,topFirst,(_:Seq[((K,This),O)]).view.map(_._1))(PrefixLoop.zipRec[K,V,(U,Seq[(K,This)])=>U,O,This](_)(((k0,this),op)))

  /** This class is used to iterate deeply through the tree.
   *  @param cur the sequence of key for the current element (from bottom to top: `cur.head=key` for deepest node)
   *  @param topFirst is true if an element appears before its children, false otherwise
   */
  protected class TreeIterator(cur:Seq[K],topFirst:Boolean) extends AbstractIterator[(Seq[K], This)] {
    protected[this] val iter = self.toIterator                 //iterator for this level
    protected[this] var i:Iterator[(Seq[K], This)] = getSub    //current sub-iterator
    protected[this] var done:Boolean = false                   //true when this item has been provided
    @tailrec final def hasNext():Boolean = !done || i!=null && (i.hasNext || {i=getSub; hasNext})
      //some clarifications: if this item has not been processed, there is a next
      //if there is no sub-iterator available and this item has been processed, we are finished
      //but if there is a sub-iterator with a next element, then there is a next
      //otherwise fetch the next sub-iterator (which could be null) and then check if it has a next element
    final def next(): (Seq[K], This) = {
      if (!done && (topFirst || i==null || !i.hasNext)) {      //give current item immediately if topFirst or wait for no more items
        done = true                                            //once processed, mark this
        (cur,self)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    final def getSub:Iterator[(Seq[K], This)] = {
      if (iter.hasNext) {                                      //move to next element
        val (k,t)=iter.next
        new t.TreeIterator(cur.+:(k),topFirst)                 //fetch sub-iterator
      } else
        null                                                   //return null when finished
    }
  }

  /** The Tree can conveniently be viewed almost as a 'Map' with sequences of keys as key.
   *  This is very convenient to iterate through it.
   *  This also provides the best way to transform a Tree: it is much easier to transform the canonical form into
   *  a new canonical form and build a new tree from that.
   */
  protected class SeqView(topFirst:Boolean) extends Iterable[(Seq[K], V)] {
    def iterator:Iterator[(Seq[K], V)] = new Iterator[(Seq[K], V)] {
      val i = new TreeIterator(Nil,topFirst)
      @tailrec def fetch:(Seq[K], This) = { if (!i.hasNext) null else { var x=i.next(); if (x._2.value.isDefined) x else fetch } }
      var cur = fetch
      def hasNext: Boolean = cur!=null
      def next(): (Seq[K], V) = { val c=cur; cur=fetch; (c._1.reverse,c._2.value.get) }
    }
    /** Transforms this seqView by applying a function to every retrieved value and key.
     *  @param  f the function used to transform the keys and values of this tree.
     *  @return a tree which maps every element of this tree.
     *          The resulting tree is a new tree.
     */
    def flatMap[W](f:V=>GenTraversable[(GenTraversable[K],W)]):GenTraversable[(GenTraversable[K],W)] =
      for (x <- iterator.toTraversable; r <- f(x._2)) yield ((x._1 ++ r._1, r._2))
  }

  /** Creates the canonical flat representation of the tree, that is the Iterable[(Seq[K], V)] that
   *  represents the collection of developed path to each node that contains a significant value.
   *  The canonical representation doesn't allow for building trees with `null` nodes (a path for
   *  which the last key results in `null`)
   *  @return the canonical view of the tree
   */
  def seqView(topFirst:Boolean=true) = new SeqView(topFirst)

  /** Appends all nodes of this tree to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string `end`.
   *  The form of `key -> node` represents the binding of a key to its associated node.
   *  `sep` is used to separate sibblings.
   *
   *  @param b     the builder to which strings are appended.
   *  @param start the starting string.
   *  @param sep   the sibblings separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    (this.asInstanceOf[TraversableOnce[(K,This)]]).map { case (k, v) => s"$k -> $v" }.addString(b, start, sep, end)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every tree is `"Map"`.
   */
  def stringPrefix: String = value match {
    case None    => "Tree"
    case Some(v) => "Tree{"+v+"}"
  }

  override def toString = {
    val b = new StringBuilder
    b.append(stringPrefix)
    addString(b,"(",",",")")
    b.toString
  }

  import PrefixLoop._

  /**generic transformations from this tree view to another tree view.
   * in all cases, v builds the values required to build the current node (null ignores that node)
   * and x builds the next data for a child (null ignores that child)
   */
  @throws(classOf[NoSuchElementException])
  def transformRec[X, L, W](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W]), x:((K, This),Seq[((K, This), X)])=> X) = (new RecurRecView[X,K,V,L,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def transformRec[L, W](k0:K, v:Seq[(K, This)]=>(L, Option[W])) = (new RecurRecViewNoData[K,V,L,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0, this)
  @throws(classOf[NoSuchElementException])
  def transform[X, L, W](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W]), x:((K, This),((K, This), X))=> X) = (new RecurView[X,K,V,L,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def transform[L, W](k0:K, v:((K, This))=>(L, Option[W])) = (new RecurViewNoData[K,V,L,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def transformRecSameKey[X, W](k0:K, x0:X, v:Seq[((K, This), X)]=>Option[W], x:((K, This),Seq[((K, This), X)])=> X) = (new RecurRecViewSameKey[X,K,V,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def transformRecSameKey[W](k0:K, v:Seq[(K, This)]=>Option[W]) = (new RecurRecViewNoDataSameKey[K,V,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def transformSameKey[X, W](k0:K, x0:X, v:(((K, This), X))=>Option[W], x:((K, This),((K, This), X))=> X) = (new RecurViewSameKey[X,K,V,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def transformSameKey[W](k0:K, v:((K, This))=>Option[W]) = (new RecurViewNoDataSameKey[K,V,W,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)

  /**generic transformations from this tree view to a PrefixTree
   */
  @throws(classOf[NoSuchElementException])
  def toTreeRec[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W], L=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecTree[X,K,V,L,W,R,This] {
    def reverseKey = null
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTreeRec[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:Seq[(K, This)]=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecTreeNoData[K,V,L,W,R,This] {
    def reverseKey = null
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTree[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W], L=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurTree[X,K,V,L,W,R,This] {
    def reverseKey = null
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTree[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:((K, This))=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurTreeNoData[K,V,L,W,R,This] {
    def reverseKey = null
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeRecSameKey[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(Option[W],K=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecTreeSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTreeRecSameKey[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:Seq[(K, This)]=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecTreeNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeSameKey[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:(((K, This), X))=>(Option[W], K=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurTreeSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTreeSameKey[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:((K, This))=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurTreeNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
}

object PrefixTraversableOnce {

  abstract class Abstract[K,+V](val value:Option[V]) extends Traversable[(K,Abstract[K,V])] with PrefixTraversableOnce[K,V,Abstract[K,V]] {
    this:Abstract[K,V] =>
    override def stringPrefix: String = super[PrefixTraversableOnce].stringPrefix
  }

  private val empty0 = new Abstract[Nothing,Nothing](None) {
    def foreach[X](f0:((Nothing,Abstract[Nothing,Nothing]))=>X):Unit = ()
    override def isNonSignificant = true
    override def isEmpty = true
    override def size = 0
  }

  def empty[K,V] = empty0.asInstanceOf[Abstract[K,V]]
}

