package utils.tree2

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

/** A generic Builder for PrefixTreeLike which extends the standard Builder class.
 *  @param empty, an empty tree
 */
abstract class PrefixTreeLikeBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]] extends Builder[(K,Tree),Tree] {
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   */
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree
  
  /** The empty value is often used */
  val empty: Tree = apply(None,Nil)
  
  /** Common uses for building various trees, most notably leaves */
  final def apply(v:Option[V]):Tree                           = apply(v,empty)
  final def apply(tree:(K,Tree)*):Tree                        = apply(None,tree)
  
  /** Build a 'Tree' using the flat form. e.g.
   *  (a,b,c) 1
   *  (a) 2
   *  (x,y) 3
   */
  final def apply(flat:Traversable[(Traversable[K],V)]):Tree =
    empty ++ (for ((k,(v,l)) <- develop(flat)) yield (k,apply(v,apply(l))))
  
  /** Implementation of the common Builder from scala libs
   */
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
}
object PrefixTreeLikeBuilder {
  implicit class Easy[K,V,Tree<:PrefixTreeLike[K,V,Tree]](b:PrefixTreeLikeBuilder[K,V,Tree]) {
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
    implicit def canBuildFrom[K, V] = new CanBuildFrom[PrefixTreeLike[_, _, _], (K, Option[V]), T[K,V]] {
      def apply(from: PrefixTreeLike[_, _, _]) = apply()
      def apply() = new Builder[(K, Option[V]), T[K,V]] {
        var e = builder[K,V].empty
        def +=(elem: (K, Option[V])) = { e += ((elem._1, builder(elem._2))); this }
        def clear(): Unit = e = builder[K,V].empty
        def result(): T[K,V] = e
      }
    }
  }
  object GenBuilder2 {
    implicit class Easy[T[k,+v]<:PrefixTreeLike[k,v,T[k,v]]](b:GenBuilder2[T]) {
      final def apply[K,V](v:V,tree:GenTraversableOnce[(K,T[K,V])]):T[K,V]   = b(Some(v),tree)
      final def apply[K,V](v:V):T[K,V]                                       = b(Some(v))
      final def apply[K,V](v:V,e:(K,T[K,V]),tree:(K,T[K,V])*):T[K,V]         = b(v,e+:tree)
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
    implicit def canBuildFrom[V] = new CanBuildFrom[PrefixTreeLike[K, _, _], (K, T[V]), T[V]] {
      def apply(from: PrefixTreeLike[K, _, _]) = builder[V]
      def apply() = builder[V]
    }
  }
  object GenBuilder1 {
    implicit class Easy[K,T[+v]<:PrefixTreeLike[K,v,T[v]]](b:GenBuilder1[K,T]) {
      final def apply[V](v:V,tree:GenTraversableOnce[(K,T[V])]):T[V] = b(Some(v),tree)
      final def apply[V](v:V):T[V]                                   = b(Some(v))
      final def apply[V](v:V,e:(K,T[V]),tree:(K,T[V])*):T[V]         = b(v,e+:tree)
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
    implicit def canBuildFrom = new CanBuildFrom[PrefixTreeLike[K, V, _], (K, T), T] {
      def apply(from: PrefixTreeLike[K, V, _]) = builder
      def apply() = builder
    }
  }
  object GenBuilder0 {
    implicit class Easy[K,V,T<:PrefixTreeLike[K,V,T]](b:GenBuilder0[K,V,T]) {
      final def apply(v:V,tree:GenTraversableOnce[(K,T)]):T = b(Some(v),tree)
      final def apply(v:V):T                                = b(Some(v))
      final def apply(v:V,e:(K,T),tree:(K,T)*):T            = b(v,e+:tree)
    }    
  }
}
