package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap
import scala.annotation.switch

/** A common use case with String as key type.
 */
abstract class StringTree[+V] extends PrefixTree[String,V] with PrefixTreeLike[String,V,StringTree[V]]

object StringTree extends PrefixTreeLikeBuilder.GenBuilder1[String,StringTree] {
  import PrefixTreeLikeBuilder.TreeParams
  
  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  implicit def build[V](implicit p:TreeParams[String,V,StringTree[V]]):PrefixTreeLikeBuilder[String, V, StringTree[V]] =
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] {
      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = {
        val i = (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0)
        (i: @switch) match {
          case 0x111 => new Abstract[V] { override def isNonSignificant = true }
          case 0x110 => new Abstract[V] { override val default = d }
          case 0x101 => new Abstract[V] { override val tree = p.emptyMap ++ t }
          case 0x100 => new Abstract[V] { override val tree = p.emptyMap ++ t; override val default = d }
          case 0x011 => new Abstract[V] { override val value = v }
          case 0x010 => new Abstract[V] { override val value = v; override val default = d }
          case 0x001 => new Abstract[V] { override val value = v; override val tree = p.emptyMap ++ t }
          case 0x000 => new Abstract[V] { override val value = v; override val tree = p.emptyMap ++ t; override val default = d }
        }
      }
    }    
  
  /** We opt to have several subclasses, depending on the situation (default or not, value or not,
   *  subtrees or not, in order to minimize the memory footprint.
   */
  protected class Abstract[V](implicit val p:TreeParams[String,V,StringTree[V]]) extends StringTree[V] {
    def newBuilder     = StringTree.build[V]
    def empty          = new Abstract[V] { override def isNonSignificant = true }
    def genericDefault = p.noDefault
  }
}
