package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap
import scala.annotation.switch

/** A common use case with String as key type.
 */
abstract class StringTree[+V] extends PrefixTree[String,V] with PrefixTreeLike[String,V,StringTree[V]]

object StringTree extends Gen1 {
  type K       = String
  type Tree[v] = StringTree[v]
  type P0[V]   = Params[V,StringTree[V]]

  /** We opt to have several subclasses, depending on the situation (default or not, value or not,
   *  subtrees or not, in order to minimize the memory footprint.
   *  note that the standard implementation uses LinkedHashMap to preserve the canonical order
   *  another map kind could be used, even an immutable one
   */
  protected class Abstract[V](implicit val params:P0[V]) extends StringTree[V] {
    type P = P0[V]
    def newBuilder = params.b2.builder[V](params)
  }
  
  /** Required parameters and appropriate default.
   */
  class Params[+V,+T<:StringTree[V] with PrefixTreeLike[String,V,T]] extends PrefixTree.Params[String,V,T] with PrefixTreeLike.Params[String,V,StringTree[V]]  {
    private[StringTree] def b2 = StringTree
  }
  object Params{
    implicit def default[V,T<:StringTree[V] with PrefixTreeLike[String,V,T]] = new Params[V,T]
  }
  
  
  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[String, V, StringTree[V]] { type P=P0[V] } = {
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] {
      type P = P0[V]
      def params:P = p
      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = {
        val i = (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0)
        (i: @switch) match {
          case 0x111 => new Abstract[V] { override def isNonSignificant = true }
          case 0x110 => new Abstract[V] { override val default = d }
          case 0x101 => new Abstract[V] { override val tree = params.emptyMap ++ t }
          case 0x100 => new Abstract[V] { override val tree = params.emptyMap ++ t; override val default = d }
          case 0x011 => new Abstract[V] { override val value = v }
          case 0x010 => new Abstract[V] { override val value = v; override val default = d }
          case 0x001 => new Abstract[V] { override val value = v; override val tree = params.emptyMap ++ t }
          case 0x000 => new Abstract[V] { override val value = v; override val tree = params.emptyMap ++ t; override val default = d }
        }
      }
    }
  }
  
  implicit final class X[V](o:StringTree.type)(implicit p:P0[V]) {
    type K = String
    type Tree = StringTree[V]
    def apply(v:Option[V],tree:GenTraversableOnce[(String,Tree)],default:K=>Tree):Tree = o.builder[V](p)(v,tree,default)
    def empty: Tree                                               = apply(None,Nil,null)
    def apply(v:Option[V]):Tree                                   = apply(v,Nil,null)
    def apply(tree:(K,Tree)*):Tree                                = apply(None,tree,null)
    def apply(default:K=>Tree):Tree                               = apply(None,Nil,default)
    def apply(default:K=>Tree,tree:(K,Tree)*):Tree                = apply(None,tree,default)
    def apply(v:Option[V],tree:GenTraversableOnce[(K,Tree)]):Tree = apply(v,tree,null)
    def apply(v:Option[V],default:K=>Tree):Tree                   = apply(v,Nil,default)
    
    def apply(v:V,tree:GenTraversableOnce[(K,Tree)],default:K=>Tree):Tree = apply(Some(v),tree,default)
    def apply(v:V,tree:GenTraversableOnce[(K,Tree)]):Tree                 = apply(Some(v),tree)
    def apply(v:V):Tree                                                   = apply(Some(v))
    def apply(v:V,default:K=>Tree):Tree                                   = apply(Some(v),default)
    def apply(v:V,tree:(K,Tree)*):Tree                                    = apply(v,tree.asInstanceOf[GenTraversableOnce[(K,Tree)]])  //fine: choice of method call
  }
  
}
