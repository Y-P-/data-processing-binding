package utils.tree2

import scala.collection.GenTraversableOnce
import scala.collection.Map
import scala.collection.mutable.LinkedHashMap

/** A common use case with String as key type.
 */
abstract class StringTree[+V] extends PrefixTree[String,V] with PrefixTreeLike[String,V,StringTree[V]]

object StringTree extends PrefixTreeLikeBuilder.GenBuilder1[String,StringTree] {
  //using LinkedHashMap as the repesentation to preserve canonical order
  implicit def builder[V] = apply[V](LinkedHashMap.empty[String, StringTree[V]])

  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  def apply[V](emptyMap: Map[String, StringTree[V]]):PrefixTreeLikeBuilder[String, V, StringTree[V]] =
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] { self=>
      class Abstract extends StringTree[V] {
        def newBuilder: PrefixTreeLikeBuilder[String, V, Repr] = self      
        def default = noDefault
      }
      //create a PrefixTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = (v==None,t.isEmpty,d==null) match {
        case (true,true,true)    => new Abstract
        case (true,true,false)   => new Abstract { override val default = d }
        case (true,false,true)   => new Abstract { override val tree = emptyMap ++ t }
        case (true,false,false)  => new Abstract { override val tree = emptyMap ++ t; override val default = d }
        case (false,true,true)   => new Abstract { override val value = v }
        case (false,true,false)  => new Abstract { override val value = v; override val default = d }
        case (false,false,true)  => new Abstract { override val value = v; override val tree = emptyMap ++ t }
        case (false,false,false) => new Abstract { override val value = v; override val tree = emptyMap ++ t; override val default = d }
      }
    }
}
