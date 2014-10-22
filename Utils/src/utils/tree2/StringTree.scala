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
  
  /** We opt to have several subclasses, depending on the situation (default or not, value or not,
   *  subtrees or not, in order to minimize the memory footprint.
   */
  protected class Abstract[V](val genericDefault:String=>Nothing) extends StringTree[V] {
    def newBuilder = StringTree(tree.empty,genericDefault)
    def empty      = Abstract.empty[V](genericDefault)
  }
  protected object Abstract {
    def empty[V](genericDefault:String=>Nothing) = new Abstract[V](genericDefault) { override def isNonSignificant = true }
  }
  
  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  def apply[V](emptyMap: Map[String, StringTree[V]],noDefault:String=>Nothing):PrefixTreeLikeBuilder[String, V, StringTree[V]] = {
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] {
      //create a PrefixTree subclass using that builder so that the Trees produced by the factory will use the same builder, hence map kind
      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = {
        val i = (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0)
        (i: @switch) match {
          case 0x111 => Abstract.empty[V](noDefault)
          case 0x110 => new Abstract[V](noDefault) { override val default = d }
          case 0x101 => new Abstract[V](noDefault) { override val tree = emptyMap ++ t }
          case 0x100 => new Abstract[V](noDefault) { override val tree = emptyMap ++ t; override val default = d }
          case 0x011 => new Abstract[V](noDefault) { override val value = v }
          case 0x010 => new Abstract[V](noDefault) { override val value = v; override val default = d }
          case 0x001 => new Abstract[V](noDefault) { override val value = v; override val tree = emptyMap ++ t }
          case 0x000 => new Abstract[V](noDefault) { override val value = v; override val tree = emptyMap ++ t; override val default = d }
        }
      }
    }
  }
}
