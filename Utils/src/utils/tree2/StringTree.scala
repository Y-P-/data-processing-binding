package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap
import scala.annotation.switch

/** A common use case with String as key type.
 */
abstract class StringTree[+V] extends PrefixTree[String,V] with PrefixTreeLike[String,V,StringTree[V]]

object StringTree extends PrefixTreeLikeBuilder.Gen1[String] {
  type Tree[+v] = StringTree[v]
  type P0[+v]   = Params[v,Tree[v]]

  protected class Abstract[V](implicit val params:P0[V]) extends StringTree[V] with super.Abstract[V]
  
  /** Required parameters and appropriate default.
   */
  abstract class Params[+V,+T<:StringTree[V] with PrefixTreeLike[String,V,T]] extends PrefixTree.Params[String,V,T] with super.Params[V,StringTree[V]]
  object Params{
    protected val p0 = new Params[Any,Tree[Any]] {
      val noDefault: Any=>Nothing = PrefixTreeLikeBuilder.noElt
      def emptyMap: scala.collection.Map[String,Tree[Any]] = LinkedHashMap.empty[String,Tree[Any]]
    }
    implicit def default[V,T<:StringTree[V] with PrefixTreeLike[String,V,T]] = p0.asInstanceOf[Params[V,T]]
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
  
}
