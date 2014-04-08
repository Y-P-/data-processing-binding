package utils.reflect

import java.lang.reflect.{Type,GenericArrayType,ParameterizedType,Modifier}
import scala.reflect.ClassTag
import scala.collection.mutable.Builder
import scala.collection.Map
import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConversions.propertiesAsScalaMap
import utils.reflect.Reflect._

/** This defines how to to spawn a collection C in an homogeneous way.
 *  This solves the problem of erasure.
 *  Note that different situations will happen:
 *  - the collection is well defined with types known (e.g. List[Int]), and the collection
 *    can be spawned (canBuildFrom found/newBuilder in companion object or static in class).
 *  - the collection is a Java Collection (from said interface) with a default constructor.
 *  - the contained type is not known (e.g. BitSet: Int is erased...), and we have to find
 *    the inner type (Int) in some way, so that the appropriate conversions will be done.
 *  - the collection is not a default Java collection (which is spawned through a default constructor)
 *    and its canBuildFrom/newBuilder cannot be found (possibly because it is a user class, an inner class or whatever.)
 *  IMPORTANT: this class relies heavily on casts ; this means that you're on your own when using it: the compiler
 *             will hardly help you locate type discrepancies!
 */
abstract class CollectionAdapter[CC:ClassTag] {
  val czzCol = implicitly[ClassTag[CC]].runtimeClass
  def apply(p:Type):BaseAdapter[_]
  
  sealed trait BaseAdapter[X] {
    type C = CC
    def isMap:Boolean
    def czCol:Type                                  //the actual collection full type
    def czElt:Type                                  //the element in the collection
    def newBuilder:Builder[X,C]                     //the builder for the collection
    def asTraversable(c:C):Traversable[X]           //making C an Traversable (useful when reading)
    final def depth(n:Int):(Int,BaseAdapter[_]) = { //go to the nth level (starting level is 1) and find the corresponding embedded adapter; all the way down if n=0
      var last:BaseAdapter[_] = this
      var depth:Int           = 1
      do {
        val d=apply(last.czElt)
        if (d==null || depth==n) return (depth,last)
        last   = d
        depth += 1
      } while (true)
      null
    }
  }
  abstract class SeqAdapter[X](val czElt:Type) extends BaseAdapter[X] {
    if (czElt==classOf[AnyRef]) throw new IllegalArgumentException(s"type $czElt is not precise enough as a collection element type: this error usually occurs when using scala primitive types (e.g. List[Int])")
    def isMap=false
    override def toString = s"SeqAdapter[$czElt]"
  }
  abstract class MapAdapter[K,X](val czKey:Class[_<:K],val czElt:Type) extends BaseAdapter[(K,X)] {
    if (czElt==classOf[AnyRef]) throw new IllegalArgumentException(s"type $czElt is not precise enough as a map element type: this error usually occurs when using scala primitive types (e.g. List[Int])")
    if (czKey==classOf[AnyRef]) throw new IllegalArgumentException(s"type $czKey is not precise enough as a map key type: this error usually occurs when using scala primitive types (e.g. Map[Int,String])")
    def isMap=true
    //you have to fill this up if canBuildFrom cannot be found as a static method, or the Java Collection class is non trivial
    def newBuilder:Builder[(K,X),C]
    override def toString = s"MapAdapter[$czKey,$czElt]"
  }
}

object CollectionAdapter {
  type Adapt = CollectionAdapter[_]#BaseAdapter[_]  
  final class Info(depth:Int,eltClass:Class[_])
  
  /** Type of the underlying contained element.
   *  Can only be called when t is associated with a container of some sort.
   *  Classes wich are not generic can get away with this if they declare a Method 'containedElementClass' that exactly returns the appropriate element type. The implementation doesn't matter.
   *  For Generic classes, the last parameter is assumed to be the contained element
   */
  final protected def eltType(t:Type):Type = {
    def error = null //throw new IllegalArgumentException(s"type $t cannot be identified as a workable collection")
    t match {
      case p:ParameterizedType     => val args = p.getActualTypeArguments; args(args.length-1)
      case g:GenericArrayType      => g.getGenericComponentType
      case a:Class[_] if a.isArray => a.getComponentType
      case c:Class[_]              => try { c.getMethod("containedElementClass").getReturnType } catch { case _:Exception => error }
      //note: Java classes will have methods with Object as signature. Not usable.
      case _ => error
    }
  }
  
  object BitSetAdapter extends CollectionAdapter[scala.collection.immutable.BitSet] {
    val a = new SeqAdapter[Int](classOf[Int]) {
      val czCol = czzCol
      def newBuilder = scala.collection.immutable.BitSet.newBuilder
      def asTraversable(c:scala.collection.immutable.BitSet):Traversable[Int] = c
    }
    def apply(p:Type) = a
  }
  object MBitSetAdapter extends CollectionAdapter[scala.collection.mutable.BitSet] {
    val a = new SeqAdapter[Int](classOf[Int]) {
      val czCol = czzCol
      def newBuilder = scala.collection.mutable.BitSet.newBuilder
      def asTraversable(c:scala.collection.mutable.BitSet):Traversable[Int] = c
    }
    def apply(p:Type) = a
  }
  object MUnrolledBuffer extends CollectionAdapter[scala.collection.mutable.UnrolledBuffer[_]] {
    class Inner[X](p:ParameterizedType) extends SeqAdapter[X](p.getActualTypeArguments()(0)) {
      val czCol = czzCol
      def newBuilder = new scala.collection.mutable.UnrolledBuffer()(ClassTag(findClass(czElt)))
      def asTraversable(c:scala.collection.mutable.UnrolledBuffer[_]):Traversable[X] = c.asInstanceOf[Traversable[X]]
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object JBitSetAdapter extends CollectionAdapter[java.util.BitSet] {
    val a = new SeqAdapter[Int](classOf[Int]) {self=>
      val czCol = czzCol
      def newBuilder = new Builder[Int,java.util.BitSet] {
        val tmp = new java.util.BitSet
        def +=(elem: Int):this.type = { tmp.set(elem); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.BitSet = tmp
      }
      def asTraversable(c:java.util.BitSet):Traversable[Int] = new Traversable[Int] {
        def foreach[U](f: Int => U):Unit = if (!c.isEmpty) {
          var i=0; val l=c.length; while (i<l) { if (c.get(i)) f(i); i+=1 }
        }
      }
    }
    def apply(p:Type) = a
  }
  object JEnumSetAdapter extends CollectionAdapter[java.util.EnumSet[_]] {
    class Inner[E<:Enum[E]](p:ParameterizedType) extends SeqAdapter[E](p.getActualTypeArguments()(0)) {
      val czCol = p
      def newBuilder = new Builder[E,java.util.EnumSet[E]] {
        val tmp = java.util.EnumSet.noneOf(czElt.asInstanceOf[Class[E]])
        def +=(elem: E):this.type = { tmp.add(elem); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.EnumSet[E] = tmp
      }
      def asTraversable(c:java.util.EnumSet[_]):Traversable[E] = {import collection.JavaConversions._ ; c.asInstanceOf[java.util.EnumSet[E]]}
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object JEnumMapAdapter extends CollectionAdapter[java.util.EnumMap[_,_]] {
    class Inner[E<:Enum[E],X](p:ParameterizedType) extends MapAdapter[E,X](p.getActualTypeArguments()(0),p.getActualTypeArguments()(1)) {
      val czCol = p
      def newBuilder = new Builder[(E,X),java.util.EnumMap[E,X]] {
        val tmp = new java.util.EnumMap[E,X](czKey.asInstanceOf[Class[E]])
        def +=(elem: (E,X)):this.type = { tmp.put(elem._1,elem._2); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.EnumMap[E,X] = tmp
      }
      def asTraversable(c:java.util.EnumMap[_,_]):Traversable[(E,X)] = {import collection.JavaConversions._ ; c.asInstanceOf[java.util.EnumMap[E,X]]}
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object JPropertiesAdapter extends CollectionAdapter[java.util.Properties] {
    val a = new MapAdapter[String,String](classOf[String],classOf[String]) {
      val czCol = czzCol
      def newBuilder = new Builder[(String,String),java.util.Properties] {
        val tmp = new java.util.Properties
        def +=(elem: (String,String)):this.type = { tmp.setProperty(elem._1,elem._2); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.Properties = tmp
      }
      def asTraversable(c:java.util.Properties):Traversable[(String,String)] = {import collection.JavaConversions._ ; c}
    }
    def apply(t:Type) = a
  }
  object IntMapAdapter extends CollectionAdapter[scala.collection.immutable.IntMap[_]] {
    class Inner[X](p:ParameterizedType) extends MapAdapter[Int,X](classOf[Int],p.getActualTypeArguments()(0)) {
      val czCol = p
      def newBuilder = scala.collection.immutable.IntMap.canBuildFrom[X,X].apply
      def asTraversable(c:scala.collection.immutable.IntMap[_]):Traversable[(Int,X)] = c.asInstanceOf[scala.collection.immutable.IntMap[X]]
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object LongMapAdapter extends CollectionAdapter[scala.collection.immutable.LongMap[_]] {
    class Inner[X](p:ParameterizedType) extends MapAdapter[Long,X](classOf[Long],p.getActualTypeArguments()(0)) {
      val czCol = p
      def newBuilder = scala.collection.immutable.LongMap.canBuildFrom[X,X].apply
      def asTraversable(c:scala.collection.immutable.LongMap[_]):Traversable[(Long,X)] = c.asInstanceOf[scala.collection.immutable.LongMap[X]]
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object HistoryAdapter extends CollectionAdapter[scala.collection.mutable.History[_,_]] {
    class Inner[X,Y](p:ParameterizedType) extends SeqAdapter[(Y,X)](classOf[Pair[Y,X]]) {
      val czCol = p
      def newBuilder = new Builder[(Y,X),scala.collection.mutable.History[X,Y]] {
        val tmp = new scala.collection.mutable.History[X,Y]
        def +=(elem: (Y,X)):this.type = { tmp.notify(elem._1,elem._2); this }
        def clear(): Unit = tmp.clear
        def result(): scala.collection.mutable.History[X,Y] = tmp
      }
      def asTraversable(c:scala.collection.mutable.History[_,_]):Traversable[(Y,X)] = c.asInstanceOf[scala.collection.mutable.History[X,Y]]
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object RevertibleHistoryAdapter extends CollectionAdapter[scala.collection.mutable.RevertibleHistory[_<:scala.collection.mutable.Undoable,_]] {
    class Inner[X<:scala.collection.mutable.Undoable,Y](p:ParameterizedType) extends SeqAdapter[(Y,X)](classOf[Pair[Y,X]]) {
      val czCol = p
      def newBuilder = new Builder[(Y,X),scala.collection.mutable.RevertibleHistory[X,Y]] {
        val tmp = new scala.collection.mutable.RevertibleHistory[X,Y]
        def +=(elem: (Y,X)):this.type = { tmp.notify(elem._1,elem._2); this }
        def clear(): Unit = tmp.clear
        def result(): scala.collection.mutable.RevertibleHistory[X,Y] = tmp
      }
      def asTraversable(c:scala.collection.mutable.RevertibleHistory[_<:scala.collection.mutable.Undoable,_]):Traversable[(Y,X)] = c.asInstanceOf[scala.collection.mutable.RevertibleHistory[X,Y]]
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
    
  /** factory to fill up the Collection adapter map ; it defaults to reflexion. */
  def apply(a:CollectionAdapter[_]*):Map[Class[_],CollectionAdapter[_]] = {
    val self = scala.collection.mutable.HashMap[Class[_],CollectionAdapter[_]]()
    for (x<-a) self += x.czzCol -> x
    self.withDefault(new ReflexiveAdapter(_))
  }
  
  
  /** Same as above, but defined by reflection where possible.
   *  This is the default when the collection class is not found in the map.
   *  @returns null if no appropriate adapter can be found
   */
  protected class ReflexiveAdapter(cz:Class[_]) extends CollectionAdapter[Any]()(ClassTag(cz)) { self=>
    def apply(t:Type):BaseAdapter[_] = {
      t match {
        case p:ParameterizedType if p.getActualTypeArguments().length==2 => eltType(p) match {
          case null => null
          case x    => new MapAdapter[Any,Any](p.getActualTypeArguments()(0),x) {
            val czCol = t
            def newBuilder:Builder[Any,Any] = self.newBuilder
            def asTraversable(c:Any):Traversable[(Any,Any)] = self.asTraversable(c).asInstanceOf[Traversable[(Any,Any)]]
          }
        }
        case _ => eltType(t) match {
          case null => null          
          case x => new SeqAdapter[Any](x) {
            val czCol = t
            def newBuilder:Builder[Any,Any] = self.newBuilder
            def asTraversable(c:Any):Traversable[Any] = self.asTraversable(c)
          }
        }
      }
    }

    /** This will be used to build a collection whenever needed */
    protected def newBuilder:Builder[Any,Any] = {
      def getJInstance[X]:X = try {
        czzCol.newInstance.asInstanceOf[X]
      } catch {
        case x:InstantiationException => if (czzCol.isInterface || Modifier.isAbstract(czzCol.getModifiers)) throw new IllegalArgumentException(s"Cannot spawn a Java collection defined by an interface or abstract class: $czzCol")
                                         throw x
      }

      if (czzCol.isArray) {
        scala.collection.mutable.ArrayBuilder.make()(ClassTag(czzCol.asInstanceOf[Class[Array[_]]].getComponentType))
      } else if (czzCol<classOf[scala.collection.MapLike[_,_,_]] || czzCol<classOf[scala.collection.Traversable[_]]) {
        //try and find canBuildFrom
        //if fails, try and find associated object
        //if found, try and find newBuilder
        //if newBuilder not found, try and find empty, then newBuilder
        (try {                       //try canBuildFrom by reflection
           czzCol.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,Any,_]].apply()
         } catch {
           case x:java.lang.NoSuchMethodException =>
             try {
               var c:RichClass[_] = null
               try {
                 c = Class.forName(czzCol.getName+"$")
                 c.c.getMethod("newBuilder").invoke(c.asObject)
               } catch {
                 case x:java.lang.NoSuchMethodException =>
                   val r = c.c.getMethod("empty").invoke(c.asObject)
                   r.getClass.getMethod("newBuilder").invoke(null)  
               }
            } catch { case x:Throwable=> 
              if (czzCol.isInterface) throw new IllegalArgumentException(s"Cannot spawn a collection defined by an interface: $czzCol")
              if (Modifier.isAbstract(czzCol.getModifiers)) throw new IllegalArgumentException(s"Cannot spawn an abstract collection unless it happens to possess a static canBuildFrom() method : $czzCol")
              throw new IllegalArgumentException(s"Collection $czzCol cannot be spawned as a stand-alone collection : ${x.getMessage}")
            }
        }).asInstanceOf[Builder[Any,Any]]
      } else if (czzCol<classOf[java.util.Map[_,_]]) {
        new Builder[Any,Any] {
          val tmp = getJInstance[java.util.Map[Any,Any]]
          def +=(elem: Any):this.type = {
            elem match {
              case a:(_,_) => tmp.put(a._1,a._2); this
              case _       => throw new IllegalArgumentException(s"a ${classOf[Pair[_,_]]} is expected when filling up a map")        
            }
          }
          def clear(): Unit = tmp.clear()
          def result(): Any = tmp
        }
      } else if (czzCol<classOf[java.util.Collection[_]]) {
        new Builder[Any,Any] {
          val tmp = getJInstance[java.util.Collection[Any]]
          def +=(elem: Any):this.type = { tmp.add(elem); this }
          def clear(): Unit = tmp.clear()
          def result(): Any = tmp
        }
      }
      else
        throw new IllegalArgumentException("Unsupported container class")
    }
    /** This will be used to build a collection whenever needed */
    def asTraversable(x:Any):Traversable[Any] = {
      import collection.JavaConversions._
      val czzCol = x.getClass
      if (czzCol.isArray) {
        x.asInstanceOf[Array[_]]
      } else if (czzCol<classOf[scala.collection.MapLike[_,_,_]]) {
        val c = x.asInstanceOf[scala.collection.MapLike[A,B,T] forSome{ type A; type B; type T<:scala.collection.MapLike[A,B,T] }]
        new Traversable[Any] {
          def foreach[U](f:Any=>U):Unit = { val i = c.iterator; while (i.hasNext) f(i.next) }
        }
      } else if (czzCol<classOf[scala.collection.Traversable[_]]) {
        x.asInstanceOf[scala.collection.Traversable[_]]
      } else if (czzCol<classOf[java.util.Map[_,_]]) {
        x.asInstanceOf[java.util.Map[_,_]]
      } else if (czzCol<classOf[java.util.Collection[_]]) {
        x.asInstanceOf[java.util.Collection[_]]
      }
      else
        throw new IllegalArgumentException("Unsupported container class")
    }
  }
  
  //the Type => Option[Adapt] function is what the user really wants ; the map is only an intermediate container
  implicit def wrap(m:Map[Class[_],CollectionAdapter[_]]):Type=>Option[Adapt] = (t:Type) => Option(m(t)(t))
  
  val defaultMap = wrap(CollectionAdapter(BitSetAdapter,JBitSetAdapter,JEnumSetAdapter,IntMapAdapter,LongMapAdapter,MBitSetAdapter,MUnrolledBuffer,HistoryAdapter,JEnumMapAdapter,JPropertiesAdapter,RevertibleHistoryAdapter))
}