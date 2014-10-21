package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap
import scala.annotation.switch

/** A common use case with String as key type.
 */
abstract class StringTree[+V] extends PrefixTree[String,V] with PrefixTreeLike[String,V,StringTree[V]]

object StringTree extends PrefixTreeLikeBuilder.GenBuilder1[String,StringTree] {
  //using LinkedHashMap as the repesentation to preserve canonical order
  implicit def builder[V] = apply[V](LinkedHashMap.empty[String, StringTree[V]],PrefixTreeLikeBuilder.noElt)
  
  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  def apply[V](emptyMap: Map[String, StringTree[V]],noDefault0:String=>StringTree[V]):PrefixTreeLikeBuilder[String, V, StringTree[V]] = {
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] {
      class Abstract extends StringTree[V] {
        def newBuilder = StringTree(tree.empty,noDefault)
        def default:String=>Repr = noDefault
      }
      def noDefault = noDefault0
      //create a PrefixTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = {
        val i = (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0)
        (i: @switch) match {
          case 0x111 => new Abstract { override def isNonSignificant = true }
          case 0x110 => new Abstract { override val default = d }
          case 0x101 => new Abstract { override val tree = emptyMap ++ t }
          case 0x100 => new Abstract { override val tree = emptyMap ++ t; override val default = d }
          case 0x011 => new Abstract { override val value = v }
          case 0x010 => new Abstract { override val value = v; override val default = d }
          case 0x001 => new Abstract { override val value = v; override val tree = emptyMap ++ t }
          case 0x000 => new Abstract { override val value = v; override val tree = emptyMap ++ t; override val default = d }
        }
      }
    }
  }
}
