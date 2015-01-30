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

object StringTree extends PrefixTreeLikeBuilder.Gen1[String] {
  type Tree[+v] = StringTree[v]
  type P0[+v]   = Params[v,Tree[v]]

  /** The full actual StringTree class used. It is designed to be sub-classed to minimize memory footprint.
   */
  class Abstract[V] protected (implicit val params:P0[V]) extends StringTree[V] with super.Abstract[V] {
    def tree: Map[K,Repr] = params.emptyMap
    override def isNonSignificant = false
    override def newBuilder = super[Abstract].newBuilder
  }

  /** The second implementation for navigable trees. Unsafe
   */
  protected class NavigableUnsafe[V](override val value:Option[V], override val tree:Map[K,StringTree[V]], override val default:K=>StringTree[V])(implicit params:P0[V])
                     extends Abstract[V] with PrefixTreeLikeBuilder.Navigable[K, V, StringTree[V]]

  /** The second implementation for navigable trees. Safe
   */
  protected class NavigableSafe[V](value:Option[V], data:Iterable[(K,StringTree[V])], default0:K=>StringTree[V])(implicit params:P0[V])
                     extends NavigableUnsafe[V](value,null,default0) {
    override val tree = params.emptyMap ++ (data.map(x=>(x._1,rebuild(x._2))))
    override val default = (k:K) => rebuild(default0(k))
    def rebuild(t:StringTree[V]):StringTree[V] = if (t.isNavigable) { val r=new NavigableUnsafe(t.value,t.tree,t.default); r.parent0=this; r } else t
    override def initNav() = ()
  }

  protected class Ref[V](valuex:Option[V], defaultx:K=>StringTree[V], val state:Int, originx: => StringTree[V], val path:K*)(implicit params:P0[V]) extends Abstract[V] with PrefixTreeLikeBuilder.Ref[K,V,StringTree[V]] {
    lazy val origin   = originx
    override def tree = target.tree
    override def isRef = super[Ref].isRef
  }



  /** The actual Parameters required to build a StringTree.
   *  These are similar with what is required for a PrefixTree, but the class actually differ.
   */
  class Params[+V,+T<:Tree[V] with PrefixTreeLike[K,V,T]](noDefault:K=>T,stripEmpty:Boolean,navigable:PrefixTreeLike.NavigableMode,mapKind:scala.collection.Map[K,T])
        extends super.Params[V,T](noDefault,stripEmpty,navigable) {
    def emptyMap: scala.collection.Map[K,T] = mapKind.empty
  }
  object Params {
    //the default value implies the LinkedHashMap Map implementation ; we share it.
    //The cast is OK because in this case, neither V nor K are actually used.
    protected val p0 = new Params[Any,Tree[Any]](PrefixTreeLikeBuilder.noElt,true,PrefixTreeLike.nonNavigable,LinkedHashMap.empty[K,Tree[Any]])
    implicit def default[V,T<:Tree[V] with PrefixTreeLike[K,V,T]] = p0.asInstanceOf[Params[V,T]]
  }

  /** A factory for working with varied map kinds if necessary.
   *  We choose to internally subclass StringTree so as to minimize the memory footprint.
   */
  implicit def builder[V](implicit p:P0[V]):PrefixTreeLikeBuilder[String, V, StringTree[V]] { type Params=P0[V] } = {
    new PrefixTreeLikeBuilder[String, V, StringTree[V]] {
      type Params = P0[V]
      def params:Params = p
      def newEmpty:PrefixTreeLikeBuilder[K,V,StringTree[V]] = builder[V]

      def asRef(value:Option[V],default:K=>Repr,origin: =>Repr, path:K*):Repr = new Ref[V](value,default,0x11,origin,path:_*)
      def asRef(default:K=>Repr,origin: =>Repr, path:K*):Repr = new Ref[V](null,default,0x01,origin,path:_*)
      def asRef(value:Option[V], origin: =>Repr, path:K*):Repr = new Ref[V](value,null,0x10,origin,path:_*)
      def asRef(origin: =>Repr, path:K*):Repr = new Ref[V](null,null,0,origin,path:_*)

      def apply(v: Option[V], t: GenTraversableOnce[(String, StringTree[V])], d: String=>StringTree[V]) = {
        val t0 = params.emptyMap ++ t
        val t1 = if (p.stripEmpty) t0.filterNot(_._2.isNonSignificant) else t0
        (if (params.navigable.id>0) -params.navigable.id else (if (v==None) 0x100 else 0)+(if (t.isEmpty) 0x10 else 0)+(if (d==null) 0x1 else 0): @switch) match {
          case 0x111 => new Abstract[V] { override def isNonSignificant = true }
          case 0x110 => new Abstract[V] { override val default = d }
          case 0x101 => new Abstract[V] { override val tree = t1 }
          case 0x100 => new Abstract[V] { override val tree = t1; override val default = d }
          case 0x011 => new Abstract[V] { override val value = v }
          case 0x010 => new Abstract[V] { override val value = v; override val default = d }
          case 0x001 => new Abstract[V] { override val value = v; override val tree = t1 }
          case 0x000 => new Abstract[V] { override val value = v; override val tree = t1; override val default = d }
          case -1    => new NavigableUnsafe[V](v,t1,d)
          case -2    => new NavigableSafe[V](v,t1,d)
        }
      }
    }
  }

  implicit def toBuilder[K,V](x:StringTree.type)(implicit p:P0[V]) = x.builder(p)
}
