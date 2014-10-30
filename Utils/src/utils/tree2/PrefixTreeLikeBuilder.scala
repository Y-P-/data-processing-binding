package utils.tree2

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
  type P
  implicit def params:P
  
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   *  apply(None,tree,null) must return the shared value empty if tree is empty
   */
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)],default:K=>Tree):Tree
  
  /** The empty value is often used */
  def empty: Tree = apply(None,Nil,null)
  
  /** rebuilds t with a specific, different value */
  def withValue(t:Tree,v:Option[V]):Tree = apply(v,t,t.default)
  /** rebuilds t with a specific, different value ; use null to fallback on the 'default' default */
  def withDefault(t:Tree,default:K=>Tree):Tree = apply(t.value,t,default)
  
  /** an interesting tree which recursively binds to itself whatever the input.
   *  This tree only holds one value which is returned on any key sequence.
   */
  final def constant(v:V):Tree = selfDefault(apply(Some(v),Nil,null))
  
  /** an interesting tree which recursively binds to itself for default
   */
  final def selfDefault(t:Tree):Tree = {
    var e = t
    e = t.withDefault(k=>e)(this)
    e
  }
  
  /** Build a 'Tree' using the flat form. e.g.
   *  (a,b,c) 1
   *  (a) 2
   *  (x,y) 3
   *  () 4
   *  
   *  It has no default.
   */
  final def apply(flat:GenTraversableOnce[(GenTraversable[K],V)]):Tree = {
    val r = deepen(flat)
    apply(r._1,r._2,empty.default)
  } 
  
  /** utility to build the map of trees for a flat representation.
   *  such a tree has no default!
   */
  protected def deepen(flat:GenTraversableOnce[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,Tree]) = {
    val d = develop(flat)
    (d._1, for ((k,(v,l)) <- d._2) yield (k,apply(v,apply(l.reverse),empty.default)))  //put back the list in the right order
  }
  
  /** Implementation of the common Builder from scala libs
   */
  protected var elems: ArrayBuffer[(K, Tree)] = ArrayBuffer.empty
  def +=(x: (K, Tree)): this.type = { elems += x; this }
  def clear():Unit = elems = ArrayBuffer.empty
  def result: Tree = { val r=elems; clear(); empty.update(r)(this) }
  
  /** inner utility : develops one level of data by tearing out the first elt of all inner iterables.
   *  @return (value for empty GenTraversable[K] if any, subtree in which children lists are in reverse order)
   */
  protected def develop(data:GenTraversableOnce[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,(Option[V],List[(GenTraversable[K],V)])]) = {
    //We use a linked map to preserve the input order. This doesn't preclude the final representation,
    //but obviously, this means that the last entries will squash previous entries if any.
    val h = LinkedHashMap.empty[K,(Option[V],List[(GenTraversable[K],V)])]
    var v:Option[V] = None
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

  trait Secure[K,V,This<:PrefixTreeLike[K,V,This]] extends Navigable[K, V, This] { this:This=>
    abstract override def initNav():Unit = ()
  }
  
  //a class for easily defining a builder for a tree where both K and V are free
  abstract class Gen2 {
    type Tree[k,+v] <: PrefixTreeLike[k,v,Tree[k,v]]
    type P0[k,+v] <: Params[k,v,Tree[k,v]]
    
    class Params[K,+V,+T<:Tree[K,V] with PrefixTreeLike[K,V,T]] (noDefault:K=>T,stripEmpty:Boolean,navigable:Boolean)
             extends PrefixTreeLike.Params[K,V,T](noDefault,stripEmpty,navigable)  {
      private[Gen2] def b2:Gen2.this.type = Gen2.this
    }

    protected trait Abstract[K,V] {
      implicit val params:P0[K,V]
      type P = P0[K,V]
      def newBuilder = params.b2.builder(params)
    }
    
    implicit def builder[K,V](implicit p:P0[K,V]):PrefixTreeLikeBuilder[K, V, Tree[K,V]] { type P=P0[K,V] }
    def apply[K,V](v:Option[V],tree:GenTraversableOnce[(K,Tree[K,V])],default:K=>Tree[K,V])(implicit p:P0[K,V]):Tree[K,V]  = builder[K,V](p)(v,tree,default)
    def apply[K,V](v:V,tree:GenTraversableOnce[(K,Tree[K,V])])(implicit p:P0[K,V]):Tree[K,V]                               = apply(Some(v),tree)
    def apply[K,V](v:V)(implicit p:P0[K,V]):Tree[K,V]                                                                      = apply(Some(v))
    def apply[K,V](v:V,default:K=>Tree[K,V])(implicit p:P0[K,V]):Tree[K,V]                                                 = apply(Some(v),default)
    def apply[K,V](v:V,e:(K,Tree[K,V]),tree:(K,Tree[K,V])*)(implicit p:P0[K,V]):Tree[K,V]                                  = apply(Some(v),e+:tree)
    def apply[K,V](v:V,tree:GenTraversableOnce[(K,Tree[K,V])],default:K=>Tree[K,V])(implicit p:P0[K,V]):Tree[K,V]          = apply(Some(v),tree,default)
    def apply[K,V](v:Option[V])(implicit p:P0[K,V]):Tree[K,V]                                                              = apply(v,Nil,null)
    def apply[K,V](v:Option[V],tree:GenTraversableOnce[(K,Tree[K,V])])(implicit p:P0[K,V]):Tree[K,V]                       = apply(v,tree,null)
    def apply[K,V](v:Option[V],default:K=>Tree[K,V])(implicit p:P0[K,V]):Tree[K,V]                                         = apply(v,Nil,default)
    def apply[K,V](e:(K,Tree[K,V]),tree:(K,Tree[K,V])*)(implicit p:P0[K,V]):Tree[K,V]                                      = apply(None,e+:tree,null)
    def apply[K,V](default:K=>Tree[K,V])(implicit p:P0[K,V]):Tree[K,V]                                                     = apply(None,Nil,default)
    def apply[K,V](default:K=>Tree[K,V],e:(K,Tree[K,V]),tree:(K,Tree[K,V])*)(implicit p:P0[K,V]):Tree[K,V]                 = apply(None,e+:tree,default)
    
    def apply[K,V](flat:GenTraversableOnce[(GenTraversable[K],V)])(implicit p:P0[K,V]):Tree[K,V] = builder[K,V](p)(flat)
    def constant[K,V](v:V)(implicit p:P0[K,V]):Tree[K,V] = builder[K,V](p).constant(v)
    def empty[K,V](implicit p:P0[K,V]): Tree[K,V] = apply(None,Nil,null)
  }
  
  //a class for easily defining a builder for a tree where K is fixed and V is free
  abstract class Gen1[K0] {
    type K = K0
    type Tree[+v] <: PrefixTreeLike[K,v,Tree[v]]
    type P0[+v] <: Params[v,_<:Tree[v]]

    class Params[+V,+T<:Tree[V] with PrefixTreeLike[K,V,T]] (noDefault:K=>T,stripEmpty:Boolean,navigable:Boolean)
             extends PrefixTreeLike.Params[K,V,T](noDefault,stripEmpty,navigable)  {
      private[Gen1] def b1:Gen1.this.type = Gen1.this
    }

    protected trait Abstract[V] {
      implicit val params:P0[V]
      type P = P0[V]
      def newBuilder = params.b1.builder[V](params)
    }

    implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[K, V, Tree[V]] { type P=P0[V] }
    def apply[V](v:Option[V],tree:GenTraversableOnce[(K,Tree[V])],default:K=>Tree[V])(implicit p:P0[V]):Tree[V]    = builder[V](p)(v,tree,default)
    def apply[V](v:V,tree:GenTraversableOnce[(K,Tree[V])])(implicit p:P0[V]):Tree[V]                               = apply(Some(v),tree)
    def apply[V](v:V)(implicit p:P0[V]):Tree[V]                                                                    = apply(Some(v))
    def apply[V](v:V,default:K=>Tree[V])(implicit p:P0[V]):Tree[V]                                                 = apply(Some(v),default)
    def apply[V](v:V,e:(K,Tree[V]),tree:(K,Tree[V])*)(implicit p:P0[V]):Tree[V]                                    = apply(Some(v),e+:tree)
    def apply[V](v:V,tree:GenTraversableOnce[(K,Tree[V])],default:K=>Tree[V])(implicit p:P0[V]):Tree[V]            = apply(Some(v),tree,default)
    def apply[V](v:Option[V])(implicit p:P0[V]):Tree[V]                                                            = apply(v,Nil,null)
    def apply[V](v:Option[V],tree:GenTraversableOnce[(K,Tree[V])])(implicit p:P0[V]):Tree[V]                       = apply(v,tree,null)
    def apply[V](v:Option[V],default:K=>Tree[V])(implicit p:P0[V]):Tree[V]                                         = apply(v,Nil,default)
    def apply[V](e:(K,Tree[V]),tree:(K,Tree[V])*)(implicit p:P0[V]):Tree[V]                                        = apply(None,e+:tree,null)
    def apply[V](default:K=>Tree[V])(implicit p:P0[V]):Tree[V]                                                     = apply(None,Nil,default)
    def apply[V](default:K=>Tree[V],e:(K,Tree[V]),tree:(K,Tree[V])*)(implicit p:P0[V]):Tree[V]                     = apply(None,e+:tree,default)
    
    def apply[V](flat:GenTraversableOnce[(GenTraversable[K],V)])(implicit p:P0[V]):Tree[V] = builder[V](p)(flat)
    def constant[V](v:V)(implicit p:P0[V]):Tree[V] = builder[V](p).constant(v)
    def empty[V](implicit p:P0[V]): Tree[V] = apply(None,Nil,null)
  }
  
  //a class for easily defining a builder for a tree where both K and V are fixed
  abstract class Gen0[K0,V0] {
    type K = K0
    type V = V0
    type Tree <: PrefixTreeLike[K,V,Tree]
    type P0 <: Params[Tree]
    
    class Params[+T<:Tree with PrefixTreeLike[K,V,T]] (noDefault:K=>T,stripEmpty:Boolean,navigable:Boolean)
             extends PrefixTreeLike.Params[K,V,T](noDefault,stripEmpty,navigable)  {
      private[Gen0] def b0:Gen0.this.type = Gen0.this
    }

    protected trait Abstract {
      implicit val params:P0
      type P = P0
      def newBuilder = params.b0.builder(params)
    }

    implicit def builder(implicit p:P0):PrefixTreeLikeBuilder[K, V, Tree] { type P=P0 }
    def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)],default:K=>Tree)(implicit p:P0):Tree = builder(p)(v,tree,default)
    def apply(v:V,tree:GenTraversableOnce[(K,Tree)])(implicit p:P0):Tree                         = apply(Some(v),tree)
    def apply(v:V)(implicit p:P0):Tree                                                           = apply(Some(v))
    def apply(v:V,default:K=>Tree)(implicit p:P0):Tree                                           = apply(Some(v),default)
    def apply(v:V,e:(K,Tree),tree:(K,Tree)*)(implicit p:P0):Tree                                 = apply(Some(v),e+:tree)
    def apply(v:V,tree:GenTraversableOnce[(K,Tree)],default:K=>Tree)(implicit p:P0):Tree         = apply(Some(v),tree,default)
    def apply(v:Option[V])(implicit p:P0):Tree                                                   = apply(v,Nil,null)
    def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)])(implicit p:P0):Tree                 = apply(v,tree,null)
    def apply(v:Option[V],default:K=>Tree)(implicit p:P0):Tree                                   = apply(v,Nil,default)
    def apply(e:(K,Tree),tree:(K,Tree)*)(implicit p:P0):Tree                                     = apply(None,e+:tree,null)
    def apply(default:K=>Tree)(implicit p:P0):Tree                                               = apply(None,Nil,default)
    def apply(default:K=>Tree,e:(K,Tree),tree:(K,Tree)*)(implicit p:P0):Tree                     = apply(None,e+:tree,default)
    
    def apply(flat:GenTraversableOnce[(GenTraversable[K],V)])(implicit p:P0):Tree = builder(p)(flat)
    def constant(v:V)(implicit p:P0):Tree = builder(p).constant(v)
    def empty(implicit p:P0): Tree = apply(None,Nil,null)
  }
}



