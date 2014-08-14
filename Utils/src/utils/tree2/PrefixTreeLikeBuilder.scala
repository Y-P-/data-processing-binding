package utils.tree2

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce

/** A generic Builder for PrefixTreeLike which extends the standard Builder class.
 *  @param empty, an empty tree
 */
abstract class PrefixTreeLikeBuilder[K,V,Tree<:PrefixTreeLike[K,V,Tree]](val empty: Tree) extends Builder[(K,Tree),Tree] {
  /** This is the generic builder method for trees.
   *  Any Tree Builder class must implement this.
   */
  def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree
  
  /** Common uses for building various trees, most notably leaves */
  final def apply(v:V,tree:GenTraversableOnce[(K,Tree)]):Tree = apply(Some(v),tree)
  final def apply(v:V):Tree                                   = apply(Some(v),empty)
  final def apply(v:V,tree:(K,Tree)*):Tree                    = apply(Some(v),tree)
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
