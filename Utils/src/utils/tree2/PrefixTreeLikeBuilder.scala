package utils.tree2

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenSeq

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
  
  /*
  /** Common uses for building various trees, most notably leaves */
  def apply(v:Option[V]):Tree                                   = apply(v,Nil,null)
  def apply(tree:(K,Tree)*):Tree                                = apply(None,tree,null)
  def apply(default:K=>Tree):Tree                               = apply(None,Nil,default)
  def apply(default:K=>Tree,tree:(K,Tree)*):Tree                = apply(None,tree,default)
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree = apply(v,tree,null)
  def apply(v:Option[V],default:K=>Tree):Tree                   = apply(v,Nil,default)
  */
  
  /** rebuilds t with a specific, different value */
  def withValue(t:Tree,v:Option[V]):Tree = apply(v,t,t.default)
  /** rebuilds t with a specific, different value ; use null to fallback on the 'default' default */
  def withDefault(t:Tree,default:K=>Tree):Tree = apply(t.value,t,default)
  
  /** an interesting tree which recursively binds to itself whatever the input.
   *  This tree only holds one value which is returned on any key sequence.
   */
  final def constant(v:V):Tree = selfDefault(apply(Some(v),Nil,null))
  
  /** an interesting tree which recursively binds to itself for default */
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
  final def apply(flat:GenTraversable[(GenTraversable[K],V)]):Tree = {
    val r = deepen(flat)
    apply(r._1,r._2,empty.default)
  } 
  
  /** utility to build the map of trees for a flat representation.
   *  such a tree has no default!
   */
  protected def deepen(flat:GenTraversable[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,Tree]) = {
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
  protected def develop(data:GenTraversable[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,(Option[V],List[(GenTraversable[K],V)])]) = {
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
}


trait Gen1 { this:Singleton =>
  type K
  type Tree[V] <: PrefixTreeLike[K,V,Tree[V]]
  type P0[V]

  implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[K, V, Tree[V]] { type P=P0[V] }
}

object Gen1 {
  
  implicit final class X[G<:Gen1 with Singleton](val o:G){
    private type K = o.K
    private type TTree[v] = o.Tree[v]
    def apply[V](v:Option[V],tree:GenTraversableOnce[(K,TTree[V])],default:K=>TTree[V])(implicit p:o.P0[V]):TTree[V] = o.builder(p)(v,tree,default)
    def apply[V](v:Option[V])(implicit p:o.P0[V]):TTree[V]                                   = apply(v,Nil,null)
    def apply[V](tree:(K,TTree[V])*)(implicit p:o.P0[V]):TTree[V]                                = apply(None,tree,null)
    def apply[V](default:K=>TTree[V])(implicit p:o.P0[V]):TTree[V]                               = apply(None,Nil,default)
    def apply[V](default:K=>TTree[V],tree:(K,TTree[V])*)(implicit p:o.P0[V]):TTree[V]                = apply(None,tree,default)
    def apply[V](v:Option[V],tree:GenTraversableOnce[(K,TTree[V])])(implicit p:o.P0[V]):TTree[V] = apply(v,tree,null)
    def apply[V](v:Option[V],default:K=>TTree[V])(implicit p:o.P0[V]):TTree[V]                   = apply(v,Nil,default)
    /*
    def apply(v:V,tree:GenTraversableOnce[(K,Tree)],default:K=>Tree):Tree = apply(Some(v),tree,default)
    def apply(v:V,tree:GenTraversableOnce[(K,Tree)]):Tree                 = apply(Some(v),tree)
    def apply(v:V):Tree                                                   = apply(Some(v))
    def apply(v:V,default:K=>Tree):Tree                                   = apply(Some(v),default)
    def apply(v:V,tree:(K,Tree)*):Tree                                    = apply(v,tree.asInstanceOf[GenTraversableOnce[(K,Tree)]])  //fine: choice of method call
    */
  }
}
