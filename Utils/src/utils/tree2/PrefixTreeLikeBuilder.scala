package utils.tree2

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable

/** A generic Builder for PrefixTreeLike which extends the standard Builder class.
 */
abstract class PrefixTreeLikeBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]] extends Builder[(K,Tree),Tree] {
  implicit final def self:this.type = this
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   */
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)],default:K=>Tree):Tree
  
  /** You can override this if you want another default `default` method than PrefixTreeLikeBuilder.noElt */
  def noDefault:K=>Tree = PrefixTreeLikeBuilder.noElt
  /** The empty value is often used ; share it */
  val empty: Tree = apply(None,Nil,null)
  
  /** Common uses for building various trees, most notably leaves */
  def apply(v:Option[V]):Tree                                   = apply(v,Nil,null)
  def apply(tree:(K,Tree)*):Tree                                = apply(None,tree,null)
  def apply(default:K=>Tree):Tree                               = apply(None,Nil,default)
  def apply(default:K=>Tree,tree:(K,Tree)*):Tree                = apply(None,tree,default)
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree = apply(v,tree,null)
  def apply(v:Option[V],default:K=>Tree):Tree                   = apply(v,Nil,default)
  
  /** rebuilds t with a specific, different value */
  def withValue(t:Tree,v:Option[V]):Tree = apply(v,t,t.default)
  /** rebuilds t with a specific, different value ; use null to fallback on the 'default' default */
  def withDefault(t:Tree,default:K=>Tree):Tree = apply(t.value,t,default)
  
  /** an interesting tree which recursively binds to itself whatever the input.
   *  This tree only holds one value which is returned on any key sequence.
   */
  final def constant(v:V):Tree = selfDefault(apply(v))
  
  /** an interesting tree which recursively binds to itself for default */
  final def selfDefault(t:Tree):Tree = {
    var e = t
    e = t.withDefault(k=>e)
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
  protected var elems: Tree = empty
  def +=(x: (K, Tree)): this.type = { elems += x; this }
  def clear():Unit = elems = empty
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
  
  implicit final class ValueBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]](b:PrefixTreeLikeBuilder[K,V,Tree]) {
    //these methods could clash with the method with Option[V] ; we explicitely place them as a second choice
    final def apply(v:V,tree:GenTraversableOnce[(K,Tree)]):Tree = b(Some(v),tree)
    final def apply(v:V):Tree                                   = b(Some(v))
    final def apply(v:V,default:K=>Tree):Tree                   = b(Some(v),default)
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
    final def apply[K,V](default:K=>T[K,V]):T[K,V]                               = builder(None,Nil,default)
    final def apply[K,V](default:K=>T[K,V],tree:(K,T[K,V])*):T[K,V]              = builder(None,tree,default)
    final def apply[K,V](v:Option[V],default:K=>T[K,V]):T[K,V]                   = builder(v,Nil,default)
    final def apply[K,V](v:Option[V],tree:GenTraversableOnce[(K,T[K,V])],default:K=>T[K,V]):T[K,V] = builder(v,tree,default)
  }
  object GenBuilder2 {
    implicit final class ValueBuilder[T[k,+v]<:PrefixTreeLike[k,v,T[k,v]]](b:GenBuilder2[T]) {
      final def apply[K,V](v:V,tree:GenTraversableOnce[(K,T[K,V])]):T[K,V]   = b(Some(v),tree)
      final def apply[K,V](v:V):T[K,V]                                       = b(Some(v))
      final def apply[K,V](v:V,e:(K,T[K,V]),tree:(K,T[K,V])*):T[K,V]         = b(Some(v),e+:tree)
      final def apply[K,V](v:V,default:K=>T[K,V]):T[K,V]                     = b(Some(v),default)
      final def apply[K,V](v:V,tree:GenTraversableOnce[(K,T[K,V])],default:K=>T[K,V]):T[K,V] = b(Some(v),tree,default)
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
    final def apply[V](default:K=>T[V]):T[V]                               = builder(None,Nil,default)
    final def apply[V](default:K=>T[V],tree:(K,T[V])*):T[V]                = builder(None,tree,default)
    final def apply[V](v:Option[V],default:K=>T[V]):T[V]                   = builder(v,Nil,default)
    final def apply[V](v:Option[V],tree:GenTraversableOnce[(K,T[V])],default:K=>T[V]):T[V] = builder(v,tree,default)
  }
  object GenBuilder1 {
    implicit final class ValueBuilder[K,T[+v]<:PrefixTreeLike[K,v,T[v]]](b:GenBuilder1[K,T]) {
      final def apply[V](v:V,tree:GenTraversableOnce[(K,T[V])]):T[V] = b(Some(v),tree)
      final def apply[V](v:V):T[V]                                   = b(Some(v))
      final def apply[V](v:V,e:(K,T[V]),tree:(K,T[V])*):T[V]         = b(Some(v),e+:tree)
      final def apply[V](v:V,default:K=>T[V]):T[V]                   = b(Some(v),default)
      final def apply[V](v:V,tree:GenTraversableOnce[(K,T[V])],default:K=>T[V]):T[V] = b(Some(v),tree,default)
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
    final def apply(default:K=>T):T                               = builder(None,Nil,default)
    final def apply(default:K=>T,tree:(K,T)*):T                   = builder(None,tree,default)
    final def apply(v:Option[V],default:K=>T):T                   = builder(v,Nil,default)
    final def apply(v:Option[V],tree:GenTraversableOnce[(K,T)],default:K=>T):T = builder(v,tree,default)
  }
  object GenBuilder0 {
    implicit class ValueBuilder[K,V,T<:PrefixTreeLike[K,V,T]](b:GenBuilder0[K,V,T]) {
      final def apply(v:V,tree:GenTraversableOnce[(K,T)]):T = b(Some(v),tree)
      final def apply(v:V):T                                = b(Some(v))
      final def apply(v:V,e:(K,T),tree:(K,T)*):T            = b(Some(v),e+:tree)
      final def apply(v:V,default:K=>T):T                   = b(Some(v),default)
      final def apply(v:V,tree:GenTraversableOnce[(K,T)],default:K=>T):T = b(Some(v),tree,default)
    }    
  }
}
