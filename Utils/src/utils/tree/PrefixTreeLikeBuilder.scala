package utils.tree

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenSeq
import scala.runtime.AbstractPartialFunction

/** A generic Builder for PrefixTreeLike which extends the standard Builder class.
 */
abstract class PrefixTreeLikeBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]] extends Builder[(K,Tree),Tree] {
  type Repr = Tree
  type Params
  implicit def params:Params
  
  /** Implementation of the common Builder from scala libs
   */
  protected var elems: ArrayBuffer[(K, Tree)] = ArrayBuffer.empty
  def +=(x: (K, Tree)): this.type = { elems += x; this }
  def clear():Unit = elems.clear
  def result: Tree = result(None,null)
  def result(v:Option[V],default:K=>Tree):Tree = { val r=apply(v,elems,default); clear; r }
  
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   *  apply(None,tree,null) must return the shared value empty if tree is empty
   *  @param value   the value for that node
   *  @param tree    the list of children nodes with their key
   *  @param default the default function to build a child tree that is not is the above list
   *  @return a new Node
   */
  def apply(value:Option[V],tree:GenTraversableOnce[(K,Tree)],default:K=>Tree):Tree
  
  /** sipmlified factories for no value */
  def apply(tree:(K,Tree)*):Tree                                        = apply(None,tree,null)
  def apply(default:K=>Tree):Tree                                       = apply(None,Nil,default)
  def apply(default:K=>Tree,tree:(K,Tree)*):Tree                        = apply(None,tree,default)
  
  /** a list of simplified factories ; note that these are shadowed by the more generic
   *  versions below. To use them, you must help the compiler, usually by specifying the
   *  expected return type: val PrefixTree[String,Int] = PrefixTree(Some(1))
   */
  def apply(v:Option[V]):Tree                                           = apply(v,Nil,null)
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree         = apply(v,tree,null)
  def apply(v:Option[V],default:K=>Tree):Tree                           = apply(v,Nil,default)
  def apply(v:Option[V],tree:(K,Tree)*):Tree                            = apply(v,tree,null)
  
  /** simplified factories with values */
  def apply(v:V):Tree                                                   = apply(Some(v),Nil,null)
  def apply(v:V,tree:GenTraversableOnce[(K,Tree)]):Tree                 = apply(Some(v),tree,null)
  def apply(v:V,default:K=>Tree):Tree                                   = apply(Some(v),Nil,default)
  def apply(v:V,tree:(K,Tree)*):Tree                                    = apply(Some(v),tree,null)
  def apply(v:V,tree:GenTraversableOnce[(K,Tree)],default:K=>Tree):Tree = apply(Some(v),tree,default)    
  
  /** Creates a full reference to a Node .
   *  A Tree Builder class may not support this feature.
   *  @param value   the value for that node ; if null, the value of the target is used
   *  @param default the default function for the new node ; if None, the value of the target is used
   *  @param tree    the node from which the reference is built
   *  @param path    the path from 'tree' to reach the referenced node
   *  @return a new Node that holds a reference to an existing Node
   */
  def asRef(value:Option[V],default:Option[K=>Tree],origin:Tree,path:K*):Tree
  
  // Some shortcuts to build references. Not all combinations are present : there
  //  are too many for a feature that is seldom used.
  /** builds a reference with a new value (not None) and default */
  def asRef(v:V,default:K=>Tree,origin:Tree,path:K*):Tree = asRef(Some(v),Some(default),origin)
  /** builds a reference with a new default */
  def asRef(default:K=>Tree,origin:Tree,path:K*):Tree     = asRef(null,Some(default),origin)
  /** builds a reference with a new value (not None) */
  def asRef(v:V,origin:Tree,path:K*):Tree                 = asRef(Some(v),None,origin)
  /** builds a full reference -the target node is unchanged in default or value- */
  def asRef(origin:Tree,path:K*):Tree                     = asRef(null,None,origin)
  
  /** A builder of the same kind, ready to use */
  def newEmpty:PrefixTreeLikeBuilder[K,V,Tree]
  
  /** The empty value is often used */
  def empty: Tree = apply(None,Nil,null)
  
  /** rebuilds t with a specific, different value */
  def withValue(t:Tree,v:Option[V]):Tree = apply(v,t,t.default)
  /** rebuilds t with a specific, different value ; use null to fallback on the 'default' default */
  def withDefault(t:Tree,default:K=>Tree):Tree = apply(t.value,t,default)
  
  /** an interesting tree which recursively binds to itself whatever the input.
   *  This tree only holds one value which is returned on any key sequence.
   */
  final def constant(v:V):Tree = apply(Some(v),Nil,null).selfDefault
    
  /** Build a 'Tree' using the flat form. e.g.
   *  (a,b,c) 1
   *  (a) 2
   *  (x,y) 3
   *  () 4
   *  
   *  It has no default.
   */
  final def fromFlat(flat:GenTraversableOnce[(GenTraversable[K],V)]):Tree = {
    val r = deepen(flat)
    apply(r._1,r._2,empty.default)
  } 
  
  /** utility to build the map of trees for a flat representation.
   *  such a tree has no default!
   */
  protected def deepen(flat:GenTraversableOnce[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,Tree]) = {
    val d = develop(flat)
    (d._1, for ((k,(v,l)) <- d._2) yield (k,apply(v,fromFlat(l.reverse),empty.default)))  //put back the list in the right order
  }
  
  /** Similar to the above, but it allows for defaults.
   *  However, it assumes that V cannot normally be null (i.e. null is None)
   */
  final def fromFlat2(flat:GenTraversableOnce[(GenTraversable[K],(V,K=>Tree))]):Tree = {
    val r = deepen2(flat)
    apply2(r._1,r._2)
  }
  
  /** utility to build the map of trees for a flat representation with default.
   */
  protected def deepen2(flat:GenTraversableOnce[(GenTraversable[K],(V,K=>Tree))]):(Option[(V,K=>Tree)],LinkedHashMap[K,Tree]) = {
    val d = develop(flat)
    (d._1, for ((k,(v,l)) <- d._2) yield (k,apply2(v,fromFlat2(l.reverse))))  //put back the list in the right order
  }
  
  /** Utility to open up (V,K=>Tree) and build a Tree node
   */
  protected def apply2(w:Option[(V, K => Tree)], l:GenTraversableOnce[(K,Tree)]) = {
    val (v,d) = w match {
      case Some((v1,d1)) => (Option(v1),d1)
      case None          => (None,null)
    }
    apply(v,l,d)    
  }
  
  /** inner utility : develops one level of data by tearing out the first elt of all inner iterables.
   *  @return (value for empty GenTraversable[K] if any, subtree in which children lists are in reverse order)
   */
  protected def develop[X](data:GenTraversableOnce[(GenTraversable[K],X)]):(Option[X],LinkedHashMap[K,(Option[X],List[(GenTraversable[K],X)])]) = {
    //We use a linked map to preserve the input order. This doesn't preclude the final representation,
    //but obviously, this means that the last entries will squash previous entries if any.
    val h = LinkedHashMap.empty[K,(Option[X],List[(GenTraversable[K],X)])]
    var v:Option[X] = None
    for (x <- data) {
      if (x._1.isEmpty) { //deal with the case where the seq is empty: we fetch the value
        v = Some(x._2)
      } else {
        val first=x._1.head
        val value=(x._1.tail,x._2)
        h.put(first,(value._1.isEmpty,h.get(first)) match {
          case (false,None)    => (None,List(value))    //create entry: intermediate leaf, init first child
          case (true, None)    => (Some(value._2),Nil)  //create entry: final leaf, put value, no children
          case (false,Some(l)) => (l._1,value::l._2)    //update entry: intermediate leaf, keep current value, update children
          case (true, Some(l)) => (Some(value._2),l._2) //update entry: final leaf, put value, keep children
        })
      }
    }
    (v,h) //note that children lists are in reverse order
  }  
  
}
object PrefixTreeLikeBuilder {
  val noElt = (x:Any) => throw new NoSuchElementException(s"key not found $x")

  /** This trait provides an easy way to build navigable trees, but there are other ways
   *  to achieve the same results.
   *  Note that this trait is unsafe (shared subtrees will have only the last parent assigned)
   *  but is can be used in a safe way.
   *  @see PrefixTree to understand how it is used in both ways (safe and unsafe.)
   */
  trait Navigable[K,V,This<:PrefixTreeLike[K,V,This]] extends PrefixTreeLike[K, V, This] { this:This=>
    var parent0:Repr with Navigable[K, V, This] = _
    override def parent:Repr = parent0
    override def isNavigable = true
    override def depth:Int = if (parent0==null) 0 else 1+parent0.depth
    override def isNonSignificant = value.isEmpty && isEmpty && default==null
    def detach():Unit = parent0=null.asInstanceOf[Repr with Navigable[K, V, This]]
    def attach(parent:Repr with Navigable[K, V, This]):Unit = parent0=parent
    abstract override def -(key:K):Repr = {
      //on removal, clear the parent
      get(key) match {
        case Some(n) if n.isInstanceOf[Navigable[K,V,This]]=> n.asInstanceOf[Navigable[K,V,This]].detach()
        case _ =>
      }
      super.-(key)
    }
    def initNav():Unit = {
      //on init, attach all Navigable to this node
      for (x<-this if x._2.isInstanceOf[Navigable[K,V,This]]) x._2.asInstanceOf[Navigable[K,V,This]].attach(this)
    }
    initNav()
  }

  /** This trait is used to ensure that the parent is not mutable.
   */
  trait Secure[K,V,This<:PrefixTreeLike[K,V,This]] extends Navigable[K, V, This] { this:This=>
    abstract override def initNav():Unit = ()
  }
  
  /** This trait is used to create references to other nodes
   */
  trait Ref[K,+V,+This<:PrefixTreeLike[K,V,This]] { this:This=>
    protected val origin:This //lazy because the likehood is a reference within the same tree
    protected val path:Seq[K]
    protected[this] lazy val target:This = origin(path:_*) //lazy because we cannot evaluate this before the tree is built
    override def get(key: K) = target.get(key)
    override def iterator    = target.iterator
    override def value       = target.value
    override def default     = target.default
  }
  
  /** Defines a builder for a tree type where both K and V are free */
  abstract class Gen2 {
    type Tree[k,+v] <: PrefixTreeLike[k,v,Tree[k,v]]
    type P0[k,+v] <: Params[k,v,Tree[k,v]]
    
    class Params[K,+V,+T<:Tree[K,V] with PrefixTreeLike[K,V,T]] (noDefault:K=>T,stripEmpty:Boolean,navigable:PrefixTreeLike.NavigableMode)
             extends PrefixTreeLike.Params[K,V,T](noDefault,stripEmpty,navigable)  {
      private[Gen2] def b2:Gen2.this.type = Gen2.this
    }

    protected trait Abstract[K,V] {
      implicit val params:P0[K,V]
      type Params = P0[K,V]
      def newBuilder = params.b2.builder(params)
    }
    
    def constant[K,V](v:V)(implicit p:P0[K,V]) = builder.constant(v)
    
    def builder[K,V](implicit p:P0[K,V]):PrefixTreeLikeBuilder[K, V, Tree[K,V]] { type Params=P0[K,V] }
    implicit def toBuilder[K,V](g:this.type)(implicit p:P0[K,V]) = g.builder(p)
  }
  
  /** Defines a builder for a tree type where K is fixed and V is free */
  abstract class Gen1[K0] {
    type K = K0
    type Tree[+v] <: PrefixTreeLike[K,v,Tree[v]]
    type P0[+v] <: Params[v,Tree[v]]

    class Params[+V,+T<:Tree[V] with PrefixTreeLike[K,V,T]] (noDefault:K=>T,stripEmpty:Boolean,navigable:PrefixTreeLike.NavigableMode)
             extends PrefixTreeLike.Params[K,V,T](noDefault,stripEmpty,navigable)  {
      private[Gen1] def b1:Gen1.this.type = Gen1.this
    }

    protected trait Abstract[V] {
      implicit val params:P0[V]
      type Params = P0[V]
      def newBuilder = params.b1.builder[V](params)
    }

    def constant[V](v:V)(implicit p:P0[V]) = builder.constant(v)
    
    def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[K, V, Tree[V]] { type Params=P0[V] }
    implicit def toBuilder[V](g:this.type)(implicit p:P0[V]) = g.builder(p)
  }
  
  /** Defines a builder for a tree type where both K and V are fixed */
  abstract class Gen0[K0,V0] {
    type K = K0
    type V = V0
    type Tree <: PrefixTreeLike[K,V,Tree]
    type P0 <: Params[Tree]
    
    class Params[+T<:Tree with PrefixTreeLike[K,V,T]] (noDefault:K=>T,stripEmpty:Boolean,navigable:PrefixTreeLike.NavigableMode)
             extends PrefixTreeLike.Params[K,V,T](noDefault,stripEmpty,navigable)  {
      private[Gen0] def b0:Gen0.this.type = Gen0.this
    }

    protected trait Abstract {
      implicit val params:P0
      type Params = P0
      def newBuilder = params.b0.builder(params)
    }
    
    def constant(v:V)(implicit p:P0) = builder.constant(v)

    def builder(implicit p:P0):PrefixTreeLikeBuilder[K, V, Tree] { type Params=P0 }
    implicit def toBuilder(g:this.type)(implicit p:P0) = g.builder(p)    
  }
}



