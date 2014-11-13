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
abstract class PrefixIterableLikeBuilder[K,V,Tree<:PrefixIterableLike[K,V,Tree]] extends Builder[(K,Tree),Tree] {
  type P
  implicit def params:P
  
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   *  apply(None,tree) must return the shared value empty if tree is empty
   */
  def apply(v:Option[V],tree:Iterable[(K,Tree)]):Tree
  
  /** A similar builder, ready to use
   */
  def newEmpty:PrefixIterableLikeBuilder[K,V,Tree]
  
  /** The empty value is often used */
  def empty: Tree = apply(None,Nil)
  
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
    apply(r._1,r._2.toSeq.map(x=>(x._1,x._2)))
  } 
  
  /** utility to build the map of trees for a flat representation.
   *  such a tree has no default!
   */
  protected def deepen(flat:GenTraversableOnce[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,Tree]) = {
    val d = develop(flat)
    (d._1, for ((k,(v,l)) <- d._2) yield (k,apply(v,apply(l.reverse))))  //put back the list in the right order
  }
      
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

object PrefixIterableLikeBuilder {
  val noElt = (x:Any) => throw new NoSuchElementException(s"key not found $x")
  
  //a class for easily defining a builder for a tree where both K and V are free
  abstract class Gen2 {
    type Tree[k,+v] <: PrefixIterableLike[k,v,Tree[k,v]]
        
    implicit def builder[K,V]:PrefixIterableLikeBuilder[K, V, Tree[K,V]]
    def apply[K,V](v:Option[V],tree:Iterable[(K,Tree[K,V])]):Tree[K,V]            = builder[K,V](v,tree)
    def apply[K,V](v:V,tree:Iterable[(K,Tree[K,V])]):Tree[K,V]                    = apply(Some(v),tree)
    def apply[K,V](v:V):Tree[K,V]                                                 = apply(Some(v))
    def apply[K,V](v:V,e:(K,Tree[K,V]),tree:(K,Tree[K,V])*):Tree[K,V]             = apply(Some(v),e+:tree)
    def apply[K,V](v:Option[V]):Tree[K,V]                                         = apply(v,Nil)
    def apply[K,V](e:(K,Tree[K,V]),tree:(K,Tree[K,V])*):Tree[K,V]                 = apply(None,e+:tree)
    
    def apply[K,V](flat:GenTraversableOnce[(GenTraversable[K],V)]):Tree[K,V] = builder[K,V](flat)
    def empty[K,V]: Tree[K,V] = apply(None,Nil)
  }
  
  //a class for easily defining a builder for a tree where K is fixed and V is free
  abstract class Gen1[K0] {
    type K = K0
    type Tree[+v] <: PrefixIterableLike[K,v,Tree[v]]

    implicit def builder[V]:PrefixIterableLikeBuilder[K, V, Tree[V]]
    def apply[V](v:Option[V],tree:Iterable[(K,Tree[V])]):Tree[V]              = builder[V](v,tree)
    def apply[V](v:V,tree:Iterable[(K,Tree[V])]):Tree[V]                      = apply(Some(v),tree)
    def apply[V](v:V):Tree[V]                                                 = apply(Some(v))
    def apply[V](v:V,e:(K,Tree[V]),tree:(K,Tree[V])*):Tree[V]                 = apply(Some(v),e+:tree)
    def apply[V](v:Option[V]):Tree[V]                                         = apply(v,Nil)
    def apply[V](e:(K,Tree[V]),tree:(K,Tree[V])*):Tree[V]                     = apply(None,e+:tree)
    
    def apply[V](flat:GenTraversableOnce[(GenTraversable[K],V)]):Tree[V] = builder[V](flat)
    def empty[V]: Tree[V] = apply(None,Nil)
  }
  
  //a class for easily defining a builder for a tree where both K and V are fixed
  abstract class Gen0[K0,V0] {
    type K = K0
    type V = V0
    type Tree <: PrefixIterableLike[K,V,Tree]
    
    implicit def builder:PrefixIterableLikeBuilder[K, V, Tree]
    def apply(v:Option[V],tree:Iterable[(K,Tree)]):Tree           = builder(v,tree)
    def apply(v:V,tree:Iterable[(K,Tree)]):Tree                   = apply(Some(v),tree)
    def apply(v:V):Tree                                           = apply(Some(v))
    def apply(v:V,e:(K,Tree),tree:(K,Tree)*):Tree                 = apply(Some(v),e+:tree)
    def apply(v:Option[V]):Tree                                   = apply(v,Nil)
    def apply(e:(K,Tree),tree:(K,Tree)*):Tree                     = apply(None,e+:tree)
    
    def apply(flat:GenTraversableOnce[(GenTraversable[K],V)]):Tree = builder(flat)
    def empty: Tree = apply(None,Nil)
  }
  
}



