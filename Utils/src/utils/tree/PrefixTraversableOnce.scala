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
   *//*
  def deepFoldRight[U](u0:U,k0:K,topFirst:Boolean)(f: ((K,Repr),U) => U): U =
    new FoldDeep(u0).right(k0,this,topFirst,f)
  def deepFoldRightRec[U](u0:U,k0:K,topFirst:Boolean)(f: (Seq[(K,Repr)],U) => U): U =
    new FoldDeep(u0).rightRec(k0,this,topFirst,f)
  def deepFoldRightTreeRec[U](u0:U,k0:K,topFirst:Boolean)(op: PrefixTreeLike.Tree[K,((K,This),U) => U]): U =
    new FoldDeep(u0).rightTreeRec(k0,this,topFirst,op)*/

  def deepFoldLeft[U](u0:U,k0:K,topFirst:Boolean)(f: (U, (K,Repr)) => U): U =
    PrefixLoop.fold(u0)(deepForeach(k0))(topFirst,f)
  def deepFoldLeftRec[U](u0:U,k0:K,topFirst:Boolean)(f: (U, Seq[(K,Repr)]) => U): U =
    PrefixLoop.fold(u0)(deepForeachRec(k0))(topFirst,f)
  //same, but the function used can change for each node and is handled through a companion tree
  def deepFoldZip[U,O<:PrefixTreeLike[K,(U,(K,Repr))=>U,O]](u0:U,k0:K)(op:O with PrefixTreeLike[K,(U,(K,Repr))=>U,O]) = {
    /*
    var u = u0
    val h1:((U, (K, Repr)) => U) => ((((K, Repr), Any), => Unit) => Unit) = 
            x => if (x==null) null else (ctx,recur) => { recur; u=x(u,ctx._1) }
    PrefixLoop.zip[K,V,(U,(K,Repr))=>U,O,This](h1)(((k0,this),op))
    u
    */
    val z1:((((K, This), O), => Unit) => Any) => Unit =
      kk=>new PrefixLoop.Zip[K,V,O,This](kk).apply(((k0,this),op))
    def v = PrefixLoop.fold[U,K,This,(((K, Repr), Any))](u0)(z1)(true,null)
  }
  //same, but in addition the function used can refer to the node's parents
  def deepFoldZipRec[U,O<:PrefixTreeLike[K,(U,Seq[((K,Repr))])=>U,O]](u0:U,k0:K)(op:O with PrefixTreeLike[K,(U,Seq[((K,Repr))])=>U,O]) = {
    /*
    var u = u0
    def h(x:(U,Seq[((K,Repr))])=>U):(Seq[((K,Repr),Any)],=>Unit)=>Unit = if (x==null) null else (ctx,recur) => { recur; u=x(u,ctx.view.map(_._1)) }
    PrefixLoop.zipRec[K,V,(U,Seq[((K,Repr))])=>U,O,This](h)(((k0,this),op))
    u
    */
    val z1:((Seq[((K, This), O)], => Unit) => Any) => Unit =
      kk=>new PrefixLoop.ZipRec[K,V,O,This](kk).apply(((k0,this),op))
    def v = PrefixLoop.fold[U,K,This,Seq[((K, Repr), Any)]](u0)(z1)(true,null)
  }


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
  def deepForeach[X](k0:K)(f:((K,Repr),=>Unit)=>X) = (new PrefixLoop.LoopNoData[Unit,K,V,This] {
    def deepLoop(ctx:Context,loop:(Result=>Any)=>Unit):Result = f(ctx,loop(null))
  })((k0,this))
  def deepForeachRec[X](k0:K)(f:(Seq[(K,Repr)],=>Unit)=>X) = (new PrefixLoop.LoopRecNoData[Unit,K,V,This] {
    def deepLoop(ctx:Context,loop:(Result=>Any)=>Unit):Result = f(ctx,loop(null))
  })((k0,this))
  //same, but the function used can change for each node and is handled through a companion tree
  def deepForeachZip[X,O<:PrefixTreeLike[K,((K,Repr),=>Unit)=>X,O]](k0:K)(op:O with PrefixTreeLike[K,((K,Repr),=>Unit)=>X,O]) = {
    val f:(((K,This),O),=>Unit)=>Unit = (ctx,recur)=>ctx._2.value match {
      case None    => recur
      case Some(g) => if (g!=null) g(ctx._1,recur)
    }
    new PrefixLoop.Zip[K,V,O,This](f)(((k0,this),op))
  }
  //same, but in addition the function used can refer to the node's parents
  def deepForeachZipRec[X,O<:PrefixTreeLike[K,(Seq[((K,Repr))],=>Unit)=>X,O]](k0:K)(op:O with PrefixTreeLike[K,(Seq[((K,Repr))],=>Unit)=>X,O]) = {
    val f:(Seq[((K,This),O)],=>Unit)=>Unit = (ctx,recur)=>ctx.head._2.value match {
      case None    => recur
      case Some(g) => if (g!=null) g(ctx.view.map(_._1),recur)
    }
    new PrefixLoop.ZipRec[K,V,O,This](f)(((k0,this),op))
  }

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

  import PrefixLoop._
  //generic transformations
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

  //generic conversions to some PrefixTree
  @throws(classOf[NoSuchElementException])
  def toTreeRec[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(L, Option[W], L=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRec[X,K,V,L,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTreeRec[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:Seq[(K, This)]=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurRecNoData[K,V,L,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTree[X, L, W, R<:PrefixTreeLike[L,W,R]](k0:K, x0:X, v:(((K, This), X))=>(L, Option[W], L=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new Recur[X,K,V,L,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTree[L, W, R<:PrefixTreeLike[L,W,R]](k0:K, v:((K, This))=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = (new RecurNoData[K,V,L,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeRecSameKey[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:Seq[((K, This), X)]=>(Option[W],K=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTreeRecSameKey[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:Seq[(K, This)]=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurRecNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
  })(k0,this)
  @throws(classOf[NoSuchElementException])
  def toTreeSameKey[X, W, R<:PrefixTreeLike[K,W,R]](k0:K, x0:X, v:(((K, This), X))=>(Option[W], K=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurSameKey[X,K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
    def nextX(child: (K, This), ctx: Context): X  = x(child,ctx)
  })(((k0, this), x0))
  @throws(classOf[NoSuchElementException])
  def toTreeSameKey[W, R<:PrefixTreeLike[K,W,R]](k0:K, v:((K, This))=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = (new RecurNoDataSameKey[K,V,W,R,This] {
    def mapValues(ctx: Context): Values   = v(ctx)
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
  
}

