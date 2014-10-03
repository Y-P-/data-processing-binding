package utils.tree2

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable

/** A generic Builder for PrefixTreeLike which extends the standard Builder class.
 *  @param replace indicates the merge mode used. You want it here, implicit, because it is used by += which signature you cannot change.
 */
abstract class PrefixTreeLikeBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]](implicit val replace:Boolean) extends Builder[(K,Tree),Tree] {
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   */
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree
  
  /** The empty value is often used */
  val empty: Tree = apply(None,Nil)
  
  /** Common uses for building various trees, most notably leaves */
  final def apply(v:Option[V]):Tree     = apply(v,empty)
  final def apply(tree:(K,Tree)*):Tree  = apply(None,tree)
  
  /** Build a 'Tree' using the flat form. e.g.
   *  (a,b,c) 1
   *  (a) 2
   *  (x,y) 3
   *  () 4
   */
  final def apply(flat:GenTraversable[(GenTraversable[K],V)]):Tree = {
    val r = deepen(flat)
    apply(r._1,r._2)
  } 
  
  /** utility to build the map of trees for a flat representation
   */
  protected def deepen(flat:GenTraversable[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,Tree]) = {
    val d = develop(flat)
    (d._1, for ((k,(v,l)) <- d._2) yield (k,apply(v,apply(l.reverse))))  //put back the list in the right order
  }
  
  /** Implementation of the common Builder from scala libs
   */
  protected var elems: Tree = empty
  def +=(x: (K, Tree)): this.type = { elems += x; this }
  def clear():Unit = { elems = empty }
  def result: Tree = elems
  
  /** inner utility : develops one level of data by tearing out the first elt of all inner iterables.
   *  @return (value for empty GenTraversable[K] if any, subtree in which children lists are in reverse order)
   */
  protected def develop(data:GenTraversable[(GenTraversable[K],V)]):(Option[V],LinkedHashMap[K,(Option[V],List[(GenTraversable[K],V)])]) = {
    //We use a linked map to preserve the input order. This doesn't preclude the final representation,
    //but obviously, this means that the last entries will squash previous entries if any.
    val h = LinkedHashMap.empty[K,(Option[V],List[(GenTraversable[K],V)])]
    var v:Option[V] = None
    for (x <- data) {
      if (x._1.isEmpty) {
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
  implicit final class Easy[K,V,Tree<:PrefixTreeLike[K,V,Tree]](b:PrefixTreeLikeBuilder[K,V,Tree]) {
    //these methods could clash with the method with Option[V] ; we explicitely place them as a second choice
    final def apply(v:V,tree:GenTraversableOnce[(K,Tree)]):Tree = b(Some(v),tree)
    final def apply(v:V):Tree                                   = b(Some(v),b.empty)
    final def apply(v:V,e:(K,Tree),tree:(K,Tree)*):Tree         = b(Some(v),e+:tree)    
  }
  /** a builder pattern for tree classes where both K and V are free */
  abstract class GenBuilder2[T[k,+v]<:PrefixTreeLike[k,v,T[k,v]]] {
    implicit def builder[K,V]:PrefixTreeLikeBuilder[K,V,T[K,V]]
    final def apply[K,V](v:Option[V],tree:GenTraversableOnce[(K,T[K,V])]):T[K,V] = builder(v,tree)
    final def apply[K,V](tree:GenTraversableOnce[(K,T[K,V])]):T[K,V]             = builder(None,tree)
    final def apply[K,V](v:Option[V]):T[K,V]                                     = builder(v)
    final def apply[K,V](v:Option[V],e:(K,T[K,V]),tree:(K,T[K,V])*):T[K,V]       = builder(v,e+:tree)
    final def apply[K,V](tree:(K,T[K,V])*):T[K,V]                                = builder(None,tree)    
  }
  object GenBuilder2 {
    implicit final class Easy[T[k,+v]<:PrefixTreeLike[k,v,T[k,v]]](b:GenBuilder2[T]) {
      final def apply[K,V](v:V,tree:GenTraversableOnce[(K,T[K,V])]):T[K,V]   = b(Some(v),tree)
      final def apply[K,V](v:V):T[K,V]                                       = b(Some(v))
      final def apply[K,V](v:V,e:(K,T[K,V]),tree:(K,T[K,V])*):T[K,V]         = b(Some(v),e+:tree)
    }    
  }
  /** a builder pattern for tree classes where V is free but K fixed (i.e. StringTree) */
  abstract class GenBuilder1[K,T[+v]<:PrefixTreeLike[K,v,T[v]]]  {
    implicit def builder[V]:PrefixTreeLikeBuilder[K,V,T[V]]
    final def apply[V](v:Option[V],tree:GenTraversableOnce[(K,T[V])]):T[V] = builder(v,tree)
    final def apply[V](tree:GenTraversableOnce[(K,T[V])]):T[V]             = builder(None,tree)
    final def apply[V](v:Option[V]):T[V]                                   = builder(v)
    final def apply[V](v:Option[V],e:(K,T[V]),tree:(K,T[V])*):T[V]         = builder(v,e+:tree)
    final def apply[V](tree:(K,T[V])*):T[V]                                = builder(None,tree)    
  }
  object GenBuilder1 {
    implicit final class Easy[K,T[+v]<:PrefixTreeLike[K,v,T[v]]](b:GenBuilder1[K,T]) {
      final def apply[V](v:V,tree:GenTraversableOnce[(K,T[V])]):T[V] = b(Some(v),tree)
      final def apply[V](v:V):T[V]                                   = b(Some(v))
      final def apply[V](v:V,e:(K,T[V]),tree:(K,T[V])*):T[V]         = b(Some(v),e+:tree)
    }    
  }
  /** a builder pattern for tree classes where both K and V are fixed */
  abstract class GenBuilder0[K,V,T<:PrefixTreeLike[K,V,T]]  {
    implicit def builder:PrefixTreeLikeBuilder[K,V,T]
    final def apply(v:Option[V],tree:GenTraversableOnce[(K,T)]):T = builder(v,tree)
    final def apply(tree:GenTraversableOnce[(K,T)]):T             = builder(None,tree)
    final def apply(v:Option[V]):T                                = builder(v)
    final def apply(v:Option[V],e:(K,T),tree:(K,T)*):T            = builder(v,e+:tree)
    final def apply(tree:(K,T)*):T                                = builder(None,tree)    
  }
  object GenBuilder0 {
    implicit class Easy[K,V,T<:PrefixTreeLike[K,V,T]](b:GenBuilder0[K,V,T]) {
      final def apply(v:V,tree:GenTraversableOnce[(K,T)]):T = b(Some(v),tree)
      final def apply(v:V):T                                = b(Some(v))
      final def apply(v:V,e:(K,T),tree:(K,T)*):T            = b(Some(v),e+:tree)
    }    
  }
}
