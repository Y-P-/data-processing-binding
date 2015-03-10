package utils.tree

import scala.collection.mutable.Map
import scala.collection.GenTraversableOnce
import scala.collection.mutable.LinkedHashMap


/** This is a mutable tree implementation, where everything can change:
 *  - the sub tree
 *  - the value
 *  - the default function
 *  - entries in the sub tree
 *  It is made compatible with PrefixTree (but usual PrefixTree cannot of course downclass to Mutable...)
 */
abstract class MutablePrefixTree[K,V] protected extends PrefixTree[K,V] with MutablePrefixTreeLike[K,V,MutablePrefixTree[K,V]] {
  def value_=(v:Option[V]):Unit
  def default_=(d:K=>Repr):Unit
  def tree_=(t:Map[K,Repr]):Unit
  override def tree: Map[K,Repr]
  def update(k:K,t:Repr):Unit = if (t==null) t.tree.remove(k) else tree(k)=t
}

/** The PrefixTree object contains:
 *  - concrete implementations for PrefixTree
 *  - a factory for building the most appropriate implementation
 */
object MutablePrefixTree extends PrefixTreeLikeBuilder.Factory2i {
  type Tree[k,v] = MutablePrefixTree[k, v]
  type P0[k,v]   = Params[k,v,Tree[k, v]]
  type Bld[k,v]  = PrefixTreeLikeBuilder[k, v, Tree[k, v]]

  /** The actual Parameters required to build a PrefixTree.
   */
  class Params[K,V,T<:Tree[K,V] with PrefixTreeLike[K,V,T]](noDefault:K=>T,stripEmpty:Boolean,mapKind:scala.collection.mutable.Map[K,T])
        extends super.Params[K,V,T](noDefault,stripEmpty) {
    def emptyMap: Map[K,T] = mapKind.empty
  }
  object Params {
    protected val p0 = new Params[Any,Any,Tree[Any,Any]](PrefixTreeLikeBuilder.noElt,true,LinkedHashMap.empty[Any,Tree[Any,Any]])
    implicit def default[K,V,T<:Tree[K,V] with PrefixTreeLike[K,V,T]] = p0.asInstanceOf[Params[K,V,T]]
  }

  /** A factory for working with varied map kinds if necessary.
   */
  implicit def builder[K,V](implicit p:P0[K,V]):Bld[K, V] { type Params=P0[K,V] } =
    new PrefixTreeLikeBuilder[K, V, Tree[K, V]] { self=>
      type Params = P0[K,V]
      val params:Params = p
      def newEmpty:PrefixTreeLikeBuilder[K,V,Repr] = builder[K,V](params)

      def apply(v: Option[V], t: GenTraversableOnce[(K, Repr)], d: K=>Repr):Repr = {
        val t0 = params.emptyMap ++ t
        new MutablePrefixTree[K, V] with MutablePrefixTree.super.Abstract[K,V] {
          override var tree: Map[K,Repr] = (if (p.stripEmpty) t0.filterNot(_._2.isNonSignificant) else t0)
          override var default:K=>Repr = d
          override var value:Option[V] = v
          override def newBuilder = super[Abstract].newBuilder
          implicit val params:P0[K,V] = self.params
        }
      }
    }
}