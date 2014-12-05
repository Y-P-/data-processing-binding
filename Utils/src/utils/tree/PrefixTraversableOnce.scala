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
  import PrefixTraversableOnce._
  //an alias that concrete classes can use as shortcut to refer to themselves
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

  /** Forces this PrefixTraversableOnce into a default PrefixTree
   */
  def toPrefixTree:PrefixTree[K,V] = toTreeSameKey[V,PrefixTree[K,V]](null.asInstanceOf[K],x=>(x._2.value,null))
  def toPrefixTree[U>:V](default:K=>PrefixTree[K,U]):PrefixTree[K,U] = toTreeSameKey[U,PrefixTree[K,U]](null.asInstanceOf[K],x=>(x._2.value,default))
  def toPrefixTree[U>:V,O<:PrefixTreeLike[K,K=>PrefixTree[K,U],O]](default:O with PrefixTreeLike[K,K=>PrefixTree[K,U],O]):PrefixTree[K,U] = toTree[U,PrefixTree[K,U],O](default)

  /** filter this tree according to some criteria.
   */
  def filter[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(f:((K,This))=> Boolean, default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R =
    toTreeSameKey[U,R](k0,(x:(K,This))=>{if (f(x)) (x._2.value,default(x)) else null})
  def filterRec[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(f:(Seq[(K,Repr)])=>Boolean, default:(Seq[(K,This)])=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]) =
    toTreeRecSameKey[U,R](k0,(x:Seq[(K,This)])=>{if (f(x)) (x.head._2.value,default(x)) else null})
  def filterView(k0:K)(f:((K,This))=> Boolean):PrefixTraversableOnce.Abstract[K, V] =
    transformSameKey(k0,(x:(K,This))=>{if (f(x)) x._2.value else null})
  def filterViewRec(k0:K)(f:(Seq[(K,Repr)])=>Boolean):PrefixTraversableOnce.Abstract[K, V] =
    transformRecSameKey(k0,(x:Seq[(K,This)])=>{if (f(x)) x.head._2.value else null})

  /** map on this tree.
   */
  def map[U,R<:PrefixTreeLike[K,U,R]](k0:K)(f:V=>U,default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R =
    toTreeSameKey(k0,(x:(K,This))=>(x._2.value.flatMap(z=>Some(f(z))),default(x)))
  def mapRec[L,U,R<:PrefixTreeLike[L,U,R]](k0:K)(f:(Seq[(K,Repr)])=>(L,Option[U],L=>R))(implicit bf:PrefixTreeLikeBuilder[L,U,R]):R =
    toTreeRec(k0,(x:Seq[(K,This)])=>f(x))
  def mapView[U](k0:K)(f:V=>U):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] =
    transformSameKey(k0,(x:(K,This))=>x._2.value.flatMap(z=>Some(f(z))))
  def mapViewRec[L,U](k0:K)(f:(Seq[(K,Repr)])=>(L,Option[U])):PrefixTraversableOnce[L,U,PrefixTraversableOnce.Abstract[L,U]] =
    transformRec(k0,(x:Seq[(K,This)])=>f(x))

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
    type Data = ((K,This),T)
    val values = (cur:Data)              => (op(cur._2,cur._1._2), null)
    val next   = (child:(K,This),s:Data) => if (!strict || s._2.isDefinedAt(child._1)) try { s._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    toTreeSameKey(null.asInstanceOf[K],t,values,next)
  }
  def zipView[U,T<:PrefixTreeLike[K,_,T]](t:T,strict:Boolean,op:(T,Repr)=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    type Data = ((K,This),T)
    val values = (cur:Data)              => op(cur._2,cur._1._2)
    val next   = (child:(K,This),s:Data) => if (!strict || s._2.isDefinedAt(child._1)) try { s._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    transformSameKey(null.asInstanceOf[K],t,values,next)
  }
  def zipViewRec[U,T<:PrefixTreeLike[K,_,T]](t:T,strict:Boolean,op:Seq[((K,Repr),T)]=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    type Data = Seq[((K,This),T)]
    val values = (cur:Data)              => op(cur)
    val next   = (child:(K,This),s:Data) => if (!strict || s.head._2.isDefinedAt(child._1)) try { s.head._2(child._1) } catch { case _:NoSuchElementException => null.asInstanceOf[T] } else null.asInstanceOf[T]
    transformRecSameKey(null.asInstanceOf[K],t,values,next)
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
  def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),(T,O))
    val values = (cur:Data)              => (cur._2._2.value.flatMap(_(cur._2._1,cur._1._2)), null)
    val next   = (child:(K,This),s:Data) => strict(child._1,s._2._1,s._2._2)
    toTreeSameKey(null.asInstanceOf[K],(t,op),values,next)
  }
  def zip2View[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
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
   *  Note that this operation is the most complex for trees and it allows extensive tree transformations.
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
  def zipFullRecView[L,W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W]),O],R<:PrefixTreeLike[L,W,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,Seq[((K,This),(T,O))]=>(L,Option[W]),O]):PrefixTraversableOnce[L,W,PrefixTraversableOnce.Abstract[L,W]] = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data)              => cur.head._2._2.value.map(_(cur)).orNull
    val next   = (child:(K,This),s:Data) => strict(child._1,s.head._2._1,s.head._2._2)
    transformRec(k0,(t,op),values,next)
  }

  /** Fold left operations that descends through the subtrees.
   *  Children are evaluated before their parent.
   *  The top tree is evaluated last with k0 as key.
   */
  def deepFoldLeft[U](u0:U,k0:K,topFirst:Boolean)(f: (U, (K,Repr)) => U): U =
    new FoldDeep(u0).left(k0,this,topFirst,f)
  def deepFoldLeftRec[U](u0:U,k0:K,topFirst:Boolean)(f: (U, Seq[(K,Repr)]) => U): U =
    new FoldDeep(u0).leftRec(k0,this,topFirst,f)
  def deepFoldLeftTreeRec[U](u0:U,k0:K,topFirst:Boolean)(op: PrefixTreeLike.Tree[K,(U,(K,This)) => U]): U =
    new FoldDeep(u0).leftTreeRec(k0,this,topFirst,op)
  def deepFoldRight[U](u0:U,k0:K,topFirst:Boolean)(f: ((K,Repr),U) => U): U =
    new FoldDeep(u0).right(k0,this,topFirst,f)
  def deepFoldRightRec[U](u0:U,k0:K,topFirst:Boolean)(f: (Seq[(K,Repr)],U) => U): U =
    new FoldDeep(u0).rightRec(k0,this,topFirst,f)
  def deepFoldRightTreeRec[U](u0:U,k0:K,topFirst:Boolean)(op: PrefixTreeLike.Tree[K,((K,This),U) => U]): U =
    new FoldDeep(u0).rightTreeRec(k0,this,topFirst,op)


  /** A foreach that descends through the subtrees.
   *  Children are all evaluated within the context of their parent, and
   *  should be somewhere invoked through the =>Unit function parameter.
   *  @param k0 an initial key for the top node
   *  @param f/op, the operation to execute on each node.
   *               (K,This) : the current node and its key (or sequence of parent node, head being current node)
   *                =>Unit   : a by-name parameter that has to be evaluated somewhere to evaluate the current node children
   *               if the call uses a function tree, then for a given key, three possible cases:
   *               - matching null sub-tree: ignore that subtree
   *               - subtree with None value: don't compute that node, but go on with the children
   *               - subtree with a value: process normally
   */
  def deepForeach[X](k0:K)(f:((K,Repr),=>Unit)=>X)                                    = Deep.foreach((k0,this), f)
  def deepForeachRec[X](k0:K)(f:(Seq[(K,Repr)],=>Unit)=>X)                            = Deep.foreachRec(Seq((k0,this)), f)
  def deepForeachTree[X](k0:K)(op:PrefixTreeLike.Tree[K,((K,This),=>Unit)=>X])         = Deep.foreachTree((k0,this), op)
  def deepForeachTreeRec[X](k0:K)(op:PrefixTreeLike.Tree[K,(Seq[(K,This)],=>Unit)=>X]) = Deep.foreachTreeRec(Seq((k0,this)), op)
  
  /** This class is used to iterate deeply through the tree.
   *  @param cur the current sequence of key for the current element (from bottom to top!)
   *  @param topFirst is true if an element appears before its children, false otherwise
   */
  protected class TreeIterator(cur:Seq[K],topFirst:Boolean) extends AbstractIterator[(Seq[K], Repr)] {
    protected[this] val iter = self.toIterator                 //iterator for this level
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
        (cur,self)
      } else                                                   //if the next is not the current item, then it is the current sub-iterator next element
        i.next
    }
    final def getSub:Iterator[(Seq[K], Repr)] = {
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
      @tailrec def fetch:(Seq[K], Repr) = { if (!i.hasNext) null else { var x=i.next(); if (x._2.value.isDefined) x else fetch } }
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
  
  /** Creates the canonical flat representation of the tree.
   *  @return the canonical view of the tree
   */
  def seqView(topFirst:Boolean=true) = new SeqView(topFirst)
  
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

  //generic transformations
  @throws(classOf[NoSuchElementException])
  def transformRec[X, L, W](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W]), x:((K, This),Seq[((K, This), X)])=> X) = (new RecurRecView[X,K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
    override def nextX(child: (K, This), s: Data): X  = x(child,s)
  })(k0, x0, this)
  @throws(classOf[NoSuchElementException])
  def transformRec[L, W](k0:K, v:Seq[(K, This)]=>(L, Option[W])) = (new RecurRecViewNoData[K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
  })(k0, this)
  @throws(classOf[NoSuchElementException])
  def transform[X, L, W](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W]), x:((K, This),((K, This), X))=> X) = (new RecurView[X,K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
    override def nextX(child: (K, This), s: Data): X  = x(child,s)
  })(k0, x0, this)
  @throws(classOf[NoSuchElementException])
  def transform[L, W](k0:K, v:((K, This))=>(L, Option[W])) = (new RecurViewNoData[K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def transformRecSameKey[X, W](k0:K, x0:X, v:Seq[((K, This), X)]=>Option[W], x:((K, This),Seq[((K, This), X)])=> X) = (new RecurRecViewSameKey[X,K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
    override def nextX(child: (K, This), s: Data): X  = x(child,s)
  })(k0, x0, this)
  @throws(classOf[NoSuchElementException])
  def transformRecSameKey[W](k0:K, v:Seq[(K, This)]=>Option[W]) = (new RecurRecViewNoDataSameKey[K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def transformSameKey[X, W](k0:K, x0:X, v:(((K, This), X))=>Option[W], x:((K, This),((K, This), X))=> X) = (new RecurViewSameKey[X,K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
    override def nextX(child: (K, This), s: Data): X  = x(child,s)
  })(k0, x0, this)
  @throws(classOf[NoSuchElementException])
  def transformSameKey[W](k0:K, v:((K, This))=>Option[W]) = (new RecurViewNoDataSameKey[K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
  })(k0,this)

  //generic conversions to some PrefixTree
  @throws(classOf[NoSuchElementException])
  def toTreeRec[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W], L=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRec[X,K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
    override def nextX(child: (K, This), s: Data): X      = x(child,s)
  })(k0,x0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeRec[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:Seq[(K, This)]=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecNoData[K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTree[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W], L=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new Recur[X,K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
    override def nextX(child: (K, This), s: Data): X      = x(child,s)
  })(k0,x0,this)
  @throws(classOf[NoSuchElementException])
  def toTree[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:((K, This))=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurNoData[K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeRecSameKey[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(Option[W],K=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecSameKey[X,K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)    = v(s)
    override def nextX(child: (K, This), s: Data): X      = x(child,s)
  })(k0,x0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeRecSameKey[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:Seq[(K, This)]=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecNoDataSameKey[K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)    = v(s)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeSameKey[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:(((K, This), X))=>(Option[W], K=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurSameKey[X,K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)   = v(s)
    override def nextX(child: (K, This), s: Data): X     = x(child,s)
  })(k0,x0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeSameKey[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:((K, This))=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurNoDataSameKey[K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)   = v(s)
  })(k0,this)
}

object PrefixTraversableOnce {

  abstract class Abstract[K,+V](val value:Option[V]) extends Traversable[(K,Abstract[K,V])] with PrefixTraversableOnce[K,V,Abstract[K,V]] {
    this:Abstract[K,V] =>
    override def stringPrefix: String = super[PrefixTraversableOnce].stringPrefix
  }

  def empty[K,V] = new Abstract[K,V](None) {
    def foreach[X](f0:((K,Abstract[K,V]))=>X):Unit = ()
    override def isNonSignificant = true
    override def isEmpty = true
    override def size = 0
  }

  /** A class to define various ways to fold the whole Tree.
   *  While the same result can be achieved by using inner methods on the main class,
   *  this way makes some implementations easier.
   */
  class FoldDeep[X,K,This<:PrefixTraversableOnce[K,_,This]](x0:X) {
    var x:X = x0
    def recD(s:(K,This),f:(X,(K,This))=>X):Unit = { for (z <- s._2) recD(z,f); x=f(x,s) }
    def recDR(s:Seq[(K,This)],f:(X,Seq[(K,This)])=>X):Unit = { for (z <- s.head._2) recDR(z+:s,f); x=f(x,s) }
    def recDT(s:(K,This),o:PrefixTreeLike.Tree[K,(X,(K,This))=>X]):Unit = {
      for (z <- s._2) {
        var o1:PrefixTreeLike.Tree[K,(X,(K,This))=>X] = null
        try { o1=o(z._1) } catch { case _:NoSuchElementException=> }
        if (o1!=null) recDT(z,o1)
      }
      o.value.orNull match {
        case null =>
        case f    => x=f(x,s)
      }
    }
    def recT(s:(K,This),f:(X,(K,This))=>X):X = { x=f(x,s); for (z <- s._2) x=recT(z,f); x }
    def recTR(s:Seq[(K,This)],f:(X,Seq[(K,This)])=>X):X = { f(x,s); for (z <- s.head._2) x=recTR(z+:s,f); x }
    def recTT(s:(K,This),o:PrefixTreeLike.Tree[K,(X,(K,This))=>X]):Unit = {
       o.value.orNull match {
        case null =>
        case f    => x=f(x,s)
      }
      for (z <- s._2) {
        var o1:PrefixTreeLike.Tree[K,(X,(K,This))=>X] = null
        try { o1=o(z._1) } catch { case _:NoSuchElementException=> }
        if (o1!=null) recTT(z,o1)
      }
    }
    def rec1D(s:(K,This),f:((K,This),X)=>X):Unit = { for (z <- s._2.reversed) rec1D(z,f); x=f(s,x) }
    def rec1DR(s:Seq[(K,This)],f:(Seq[(K,This)],X)=>X):Unit = { for (z <- s.head._2.reversed) rec1DR(z+:s,f); x=f(s,x) }
    def rec1DT(s:(K,This),o:PrefixTreeLike.Tree[K,((K,This),X)=>X]):Unit = {
      for (z <- s._2.reversed) {
        var o1:PrefixTreeLike.Tree[K,((K,This),X)=>X] = null
        try { o1=o(z._1) } catch { case _:NoSuchElementException=> }
        if (o1!=null) rec1DT(z,o1)
      }
      o.value.orNull match {
        case null =>
        case f    => x=f(s,x)
      }
    }
    def rec1T(s:(K,This),f:((K,This),X)=>X):X = { x=f(s,x); for (z <- s._2.reversed) x=rec1T(z,f); x }
    def rec1TR(s:Seq[(K,This)],f:(Seq[(K,This)],X)=>X):X = { f(s,x); for (z <- s.head._2.reversed) x=rec1TR(z+:s,f); x }
    def rec1TT(s:(K,This),o:PrefixTreeLike.Tree[K,((K,This),X)=>X]):Unit = {
       o.value.orNull match {
        case null =>
        case f    => x=f(s,x)
      }
      for (z <- s._2.reversed) {
        var o1:PrefixTreeLike.Tree[K,((K,This),X)=>X] = null
        try { o1=o(z._1) } catch { case _:NoSuchElementException=> }
        if (o1!=null) rec1DT(z,o1)
      }
    }

    def left(k0:K,tree:This,topFirst:Boolean,f:(X,(K,This))=>X):X                                = { if (topFirst) recT((k0,tree),f)        else recD((k0,tree),f); x }
    def leftRec(k0:K,tree:This,topFirst:Boolean,f:(X,Seq[(K,This)])=>X):X                        = { if (topFirst) recTR(Seq((k0,tree)),f)  else recDR(Seq((k0,tree)),f); x }
    def leftTreeRec(k0:K,tree:This,topFirst:Boolean,o:PrefixTreeLike.Tree[K,(X,(K,This))=>X]):X  = { if (topFirst) recTT((k0,tree),o)       else recDT((k0,tree),o); x}
    def right(k0:K,tree:This,topFirst:Boolean,f:((K,This),X)=>X):X                               = { if (topFirst) rec1T((k0,tree),f)       else rec1D((k0,tree),f); x }
    def rightRec(k0:K,tree:This,topFirst:Boolean,f:(Seq[(K,This)],X)=>X):X                       = { if (topFirst) rec1TR(Seq((k0,tree)),f) else rec1DR(Seq((k0,tree)),f); x }
    def rightTreeRec(k0:K,tree:This,topFirst:Boolean,o:PrefixTreeLike.Tree[K,((K,This),X)=>X]):X = { if (topFirst) rec1TT((k0,tree),o)      else rec1DT((k0,tree),o); x}
  }

  /** A class to define various way to iterate through the whole tree.
   */
  object Deep {
    def foreach[X,K,This<:PrefixTraversableOnce[K,_,This]](s:(K,This),f:((K,This),=>Unit)=>X):Unit = f(s,for (z <- s._2) foreach(z,f))
    def foreachRec[X,K,This<:PrefixTraversableOnce[K,_,This]](s:Seq[(K,This)],f:(Seq[(K,This)],=>Unit)=>X):Unit = f(s,for (z <- s.head._2) foreachRec(z+:s,f))
    def foreachTree[X,K,This<:PrefixTraversableOnce[K,_,This]](s:(K,This),o:PrefixTreeLike.Tree[K,((K,This),=>Unit)=>X]):Unit = if (o!=null) {
      val recur: ()=>Unit =
        ()=> for (z <- s._2) {
          var o1:PrefixTreeLike.Tree[K,((K,This),=>Unit)=>X] = null
          try { o1=o(z._1) } catch { case _:NoSuchElementException=> }
          foreachTree(z,o1)
        }
      if (o.value!=None) o.value.get(s, recur()) else recur()
    }
    def foreachTreeRec[X,K,This<:PrefixTraversableOnce[K,_,This]](s:Seq[(K,This)],o:PrefixTreeLike.Tree[K,(Seq[(K,This)],=>Unit)=>X]):Unit = if (o!=null) {
      val recur: ()=>Unit =
        ()=> for (z <- s.head._2) {
          var o1:PrefixTreeLike.Tree[K,(Seq[(K,This)],=>Unit)=>X] = null
          try { o1=o(z._1) } catch { case _:NoSuchElementException=> }
          foreachTreeRec(z+:s,o1)
        }
      if (o.value!=None) o.value.get(s, recur()) else recur()
    }
  }

  //General note: the 16 following classes are increasingly complex versions of the last two classes.
  //The 14 first classes can be fully duplicated by the last two, albeit at a higher processing cost (expected,
  //not verified) and a very slightly higher memory footprint.
  //The last class defines the algorithm for generic recursion ; other classes just adapt it slightly.
  //For testing purposes, this means that the most important class to test is the last one.
  //The class that comes just before introduces the handling of defaults and direct translation to
  //PrefixTreeLike. That handling must also be thoroughly tested.
  //All other classes should of course be tested, but their is a high priority on the last two, in the
  //sense that there is a very strong reason to believe that once these two classes are bug-free, the other
  //will too...

  abstract class RecurBase[X,K,V,L,W,RR,This] {
    type Data
    type Values
    type R = RR
    /** Building the value for the child
     *  @param data, the current data for the node in building
     *  @return a node values, which can be null (child ignored)
     */
    def mapValues(cur:Data):Values
    /** Building the data for the child
     *  @param child, the child being built
     *  @param cur, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child:(K,This),cur:Data):X = throw new IllegalStateException
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(cur:Data,defa:K=>This,g:L=>K): L=>R = null
    /**The recursive call to walk the tree
     * @param data, the current data for that node layer
     * @return a node (key,value), which can be null (ignored)
     */
    def recur(s:Data):(L,R)
  }


  /** as Recur, simplified in cases where no external X data is involved and parents are not needed and the keys don't change
   */
  abstract class RecurNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R])
    extends RecurBase[Nothing,K,V,K,W,R,This] {
    type Data   = (K,This)
    type Values = (Option[W],K=>R)
    override def buildDefault(s:Data,defa:K=>This,g:K=>K=null): K=>R = k => {
      recur((k,defa(k))) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case r1   => r1._2
      }
    }
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._2) recur(z) match {
          case null =>
          case r1   => b += r1
        }
        (s._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = recur((k0,tree)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurView, simplified in cases where no external X data is involved and parent are not needed and the keys don't change
   */
  abstract class RecurViewNoDataSameKey[K, V, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[Nothing,K,V,K,W,Abstract[K,W],This] {
    type Data   = (K, This)
    type Values = Option[W]
    def recur(s: Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => (s._1,new R(u) {
        def foreach[X](g:((K,R))=>X):Unit = {
          for (z <- s._2) recur(z) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): R = recur((k0, tree)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRec simplified when no parents are needed and the keys don't change
   */
  abstract class RecurSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R])
    extends RecurBase[X,K,V,K,W,R,This] {
    type Data   = ((K,This),X)
    type Values = (Option[W],K=>R)
    override def buildDefault(s:Data,defa:K=>This,g:K=>K=null): K=>R = k => {
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case x1   => recur((chld,x1)) match {
          case null => throw new PrefixTreeLike.NoDefault(k)
          case r1   => r1._2
        }
      }
    }
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)) match {
            case null =>
            case r1   => b += r1
          }
        }
        (s._1._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = recur(((k0,tree),x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRecView simplified when parents are not needed and the keys don't change
   */
  abstract class RecurViewSameKey[X, K, V, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,K,W,Abstract[K,W],This] {
    type Data   = ((K, This), X)
    type Values = Option[W]
    def recur(s: Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => (s._1._1,new R(u) {
        def foreach[X](g:((K,R))=>X):Unit = {
          for (z <- s._1._2) nextX(z, s) match {
            case null =>
            case x1 => recur((z, x1)) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): R = recur(((k0, tree), x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRec, simplified in cases where no external X data is involved and the keys don't change
   */
  abstract class RecurRecNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R])
    extends RecurBase[Nothing,K,V,K,W,R,This] {
    type Data   = Seq[(K,This)]
    type Values = (Option[W],K=>R)
    override def buildDefault(s:Data,defa:K=>This,g:K=>K=null): K=>R = k => {
      recur((k,defa(k)) +: s) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case r1   => r1._2
      }
    }
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._2) recur(z+:s) match {
          case null =>
          case r1   => b += r1
        }
        (s.head._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = recur(Seq((k0,tree))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRecView, simplified in cases where no external X data is involved and the keys don't change
   */
  abstract class RecurRecViewNoDataSameKey[K, V, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[Nothing,K,V,K,W,Abstract[K,W],This] {
    type Data = Seq[(K, This)]
    type Values = Option[W]
    def recur(s: Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => (s.head._1,new R(u) {
        def foreach[X](g:((K,R))=>X):Unit = {
          for (z <- s.head._2) recur(z +: s) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): R = recur(Seq((k0, tree))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRec when the keys don't change
   */
  abstract class RecurRecSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R])
    extends RecurBase[X,K,V,K,W,R,This] {
    type Data = Seq[((K,This),X)]
    type Values = (Option[W],K=>R)
    override def buildDefault(s:Data,defa:K=>This,g:K=>K=null): K=>R = k => {
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case x1   => recur((chld,x1)+:s) match {
          case null => throw new PrefixTreeLike.NoDefault(k)
          case r1   => r1._2
        }
      }
    }
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)+:s) match {
            case null =>
            case r1   => b += r1
          }
        }
        (s.head._1._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = recur(Seq(((k0,tree),x))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRecView when the keys don't change
   */
  abstract class RecurRecViewSameKey[X, K, V, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,K,W,Abstract[K,W],This]{
    type Data = Seq[((K, This), X)]
    type Values = Option[W]
    def recur(s: Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => (s.head._1._1,new R(u) {
        def foreach[X](g:((K,R))=>X):Unit = {
          for (z <- s.head._1._2) nextX(z, s) match {
            case null =>
            case x1 => recur((z, x1) +: s) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): R = recur(Seq(((k0, tree), x))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as Recur, simplified in cases where no external X data is involved and parents are not needed
   */
  abstract class RecurNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R])
    extends RecurBase[Nothing,K,V,L,W,R,This] {
    type Data   = (K,This)
    type Values = (L,Option[W],L=>R)
    override def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = if (g==null) null else l => {
      val k = g(l)
      recur((k,defa(k))) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case r1   => r1._2
      }
    }
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._2) recur(z) match {
          case null =>
          case r1   => b += r1
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = get(k0,tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0:K,tree:This):(L,R) = recur((k0,tree)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  /** as RecurView, simplified in cases where no external X data is involved and parent are not needed
   */
  abstract class RecurViewNoData[K, V, L, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[Nothing,K,V,L,W,Abstract[L,W],This] {
    type Data = (K, This)
    type Values = (L, Option[W])
    def recur(s: Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => (u._1,new R(u._2) {
        def foreach[X](g:((L,R))=>X):Unit = {
          for (z <- s._2) recur(z) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): R = get(k0,tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0: K, tree: This): (L,R) = recur((k0, tree)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  /** As RecurRec simplified when no parents are needed
   */
  abstract class Recur[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R])
    extends RecurBase[X,K,V,L,W,R,This] {
    type Data = ((K,This),X)
    type Values = (L,Option[W],L=>R)
    override def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = if (g==null) null else l => {
      val k = g(l)
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case x1   => recur((chld,x1)) match {
          case null => throw new PrefixTreeLike.NoDefault(k)
          case r1   => r1._2
        }
      }
    }
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)) match {
            case null =>
            case r1   => b += r1
          }
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = get(k0,x,tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0:K,x:X,tree:This):(L,R) = recur(((k0,tree),x)) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  /** As RecurRecView simplified when parents are not needed
   */
  abstract class RecurView[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,L,W,Abstract[L,W],This] {
    type Data = ((K, This), X)
    type Values = (L, Option[W])
    def recur(s: Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => (u._1,new R(u._2) {
        def foreach[X](g:((L,R))=>X):Unit = {
          for (z <- s._1._2) nextX(z, s) match {
            case null =>
            case x1 => recur((z, x1)) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): R = recur(((k0, tree), x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRec, simplified in cases where no external X data is involved
   */
  abstract class RecurRecNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R])
    extends RecurBase[Nothing,K,V,L,W,R,This] {
    type Data = Seq[(K,This)]
    type Values = (L,Option[W],L=>R)
    override def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = if (g==null) null else l => {
      val k = g(l)
      recur((k,defa(k))+:s) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case r1   => r1._2
      }
    }
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._2) recur(z+:s) match {
          case null =>
          case r1   => b += r1
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = get(k0,tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0:K,tree:This):(L,R) = recur(Seq((k0,tree))) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  /** as RecurRecView, simplified in cases where no external X data is involved
   */
  abstract class RecurRecViewNoData[K, V, L, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[Nothing,K,V,L,W,Abstract[L,W],This] {
    type Data = Seq[(K, This)]
    type Values = (L, Option[W])
    def recur(s: Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => (u._1,new R(u._2) {
        def foreach[X](g:((L,R))=>X):Unit = {
          for (z <- s.head._2) recur(z +: s) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): R = get(k0, tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0: K, tree: This): (L,R) = recur(Seq((k0, tree))) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }

  /** The most generic class that highlights the recursion algorithm on PrefixTraversableOnce.
   *  Like RecurRecView, but the purpose is to build an actual PrefixTreeLike tree, not just
   *  a PrefixTraversableOnce (a view) ; it is as complex as its sibling, has most of the same
   *  properties, and adds the possibility to handle a default function.
   *  Not using RecurRecView also removes one layer of intermediate object (Abstract.)
   */
  abstract class RecurRec[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R])
    extends RecurBase[X,K,V,L,W,R,This] {
    type Data = Seq[((K,This),X)]
    type Values = (L,Option[W],L=>R)
    override def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = if (g==null) null else l => {
      val k = g(l)
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => throw new PrefixTreeLike.NoDefault(k)
        case x1   => recur((chld,x1)+:s) match {
          case null => throw new PrefixTreeLike.NoDefault(k)
          case r1   => r1._2
        }
      }
    }
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)+:s) match {
            case null =>
            case r1   => b += r1
          }
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = get(k0,x,tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0:K,x:X,tree:This):(L,R) = recur(Seq(((k0,tree),x))) match {
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
  abstract class RecurRecView[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]]
    extends RecurBase[X,K,V,L,W,Abstract[L,W],This] {
    type Data = Seq[((K, This), X)]
    type Values = (L, Option[W])
    def recur(s: Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => (u._1,new R(u._2) {
        def foreach[X](g:((L,R))=>X):Unit = {
          for (z <- s.head._1._2) nextX(z, s) match {
            case null =>
            case x1   => recur((z, x1) +: s) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): R = get(k0, x, tree)._2

    @throws(classOf[NoSuchElementException])
    def get(k0: K, x: X, tree: This): (L,R) = recur(Seq(((k0, tree), x))) match {
      case null => throw new NoSuchElementException
      case r    => r
    }
  }
}

