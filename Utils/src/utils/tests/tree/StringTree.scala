package utils.tests.tree

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap
import scala.annotation.switch
import utils.tree._

/** A common use case with String as key type.
 *  This highlights how one can derive PrefixTree for a specific case.
 *  Here, StringTree does nothing special: it would be best to just write
 *     type StringTree[+V]=PrefixTree[String,V]
 */
abstract class StringTree[+V] extends PrefixTree[String,V] with PrefixTreeLike[String,V,StringTree[V]]

object StringTree extends PrefixTreeLikeBuilder.Factory1[String] {
  type Tree[+v] = StringTree[v]
  type P0[+v]   = Params[v,Tree[v]]
  type Bld[v]   = PrefixTreeLikeBuilder[String, v, Tree[v]]

  /** The full actual StringTree class used. It is designed to be sub-classed to minimize memory footprint.
   */
  class Abstract[V] protected (implicit val params:P0[V]) extends StringTree[V] with super.Abstract[V] {
    def tree: Map[K,Repr] = params.emptyMap
    override def isNonSignificant = false
    override def newBuilder = super[Abstract].newBuilder
  }

  /** The actual Parameters required to build a StringTree.
   *  These are similar with what is required for a PrefixTree, but the class actually differ.
   */
  class Params[+V,+T<:Tree[V] with PrefixTreeLike[K,V,T]](noDefault:K=>T,stripEmpty:Boolean,mapKind:scala.collection.Map[K,T])
        extends super.Params[V,T](noDefault,stripEmpty) {
    def emptyMap: scala.collection.Map[K,T] = mapKind.empty
  }
  object Params {
    //the default value implies the LinkedHashMap Map implementation ; we share it.
    //The cast is OK because in this case, neither V nor K are actually used.
    protected val p0 = new Params[Any,Tree[Any]](PrefixTreeLikeBuilder.noElt,true,LinkedHashMap.empty[K,Tree[Any]])
    implicit def default[V,T<:Tree[V] with PrefixTreeLike[K,V,T]] = p0.asInstanceOf[Params[V,T]]
  }

  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[String, V, StringTree[V]] { type Params=P0[V] } = {
    new PrefixTreeLikeBuilder[K, V, Tree[V]] {
      type Params = P0[V]
      def params:Params = p
      def newEmpty:PrefixTreeLikeBuilder[K,V,StringTree[V]] = builder[V]

      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = {
        val t0 = params.emptyMap ++ t
        val t1 = if (p.stripEmpty) t0.filterNot(_._2.isNonSignificant) else t0
        (if (v==None) 0x100 else 0)+(if (t1.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0: @switch) match {
          case 0x111 => new Abstract[V] { override def isNonSignificant = true }
          case 0x110 => new Abstract[V] { override val default = d }
          case 0x101 => new Abstract[V] { override val tree = t1 }
          case 0x100 => new Abstract[V] { override val tree = t1; override val default = d }
          case 0x011 => new Abstract[V] { override val value = v }
          case 0x010 => new Abstract[V] { override val value = v; override val default = d }
          case 0x001 => new Abstract[V] { override val value = v; override val tree = t1 }
          case 0x000 => new Abstract[V] { override val value = v; override val tree = t1; override val default = d }
        }
      }
    }
  }
}
