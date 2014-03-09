package loader.reflect

import java.lang.reflect.{Type,GenericArrayType,ParameterizedType,Modifier}
import scala.reflect.ClassTag
import scala.collection.mutable.{HashMap,Builder,ArrayBuilder}
import scala.collection.Map
import loader.core.definition.Processor
import utils.Reflect._

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
 */
abstract class CollectionAdapter[C:ClassTag,-E<:Processor#GenElt] {
  val czzCol:Class[_] = implicitly[ClassTag[C]].runtimeClass
  def apply(p:Type):BaseAdapter[_]
  
  sealed trait BaseAdapter[-X] {
    def isMap:Boolean
    def czCol:Type                                      //the actual collection full type
    def czElt:Type                                      //the element in the collection
    def newBuilder(e:E):Builder[X,C]                    //the builder for the collection
  }
  abstract class SeqAdapter[X](val czElt:Type) extends BaseAdapter[X] {
    if (czElt==classOf[AnyRef]) throw new IllegalArgumentException(s"type $czElt is not precise enough as a collection element type: this error usually occurs when using scala primitive types (e.g. List[Int])")
    def isMap=false
    //you have to fill this up if canBuildFrom cannot be found as a static method, or the Java Collection class is non trivial
    def newBuilder(e:E):Builder[X,C]
    override def toString = s"SeqAdapter[$czElt]"
  }
  abstract class MapAdapter[K,X](val czKey:Class[_],val czElt:Type) extends BaseAdapter[(K,X)] {
    if (czElt==classOf[AnyRef]) throw new IllegalArgumentException(s"type $czElt is not precise enough as a map element type: this error usually occurs when using scala primitive types (e.g. List[Int])")
    if (czKey==classOf[AnyRef]) throw new IllegalArgumentException(s"type $czKey is not precise enough as a map key type: this error usually occurs when using scala primitive types (e.g. Map[Int,String])")
    def isMap=true
    //you have to fill this up if canBuildFrom cannot be found as a static method, or the Java Collection class is non trivial
    def newBuilder(e:E):Builder[(K,X),C]
    override def toString = s"MapAdapter[$czKey,$czElt]"
  }
}

object CollectionAdapter {
  import Binder._
  /** Type of the underlying contained element.
   *  can only be called when t is associated with a container of some sort.
   *  Classes wich are not generic can get away with this if they declare a Method 'dummyElt' that exactly returns the appropriate element type. The value doesn't matter.
   */
  final protected def eltType(t:Type):Type = {
    def error = throw new IllegalArgumentException(s"type $t cannot be identified as a workable collection")
    t match {
      case p:ParameterizedType     => val args = p.getActualTypeArguments; args(args.length-1)
      case g:GenericArrayType      => g.getGenericComponentType
      case a:Class[_] if a.isArray => a.getComponentType
      case c:Class[_]              => try { c.getMethod("czElt").getReturnType } catch { case _:Exception => error }
      //note: Java classes will have methods with Object as signature. Not usable.
      case _ => error
    }
  }
  
  object BitSetAdapter extends CollectionAdapter[scala.collection.immutable.BitSet,Processor#GenElt] {
    val a = new SeqAdapter[Int](classOf[Int]) {
      val czCol = czzCol
      def newBuilder(e:Processor#GenElt) = scala.collection.immutable.BitSet.newBuilder
    }
    def apply(p:Type) = a
  }
  object MBitSetAdapter extends CollectionAdapter[scala.collection.mutable.BitSet,Processor#GenElt] {
    val a = new SeqAdapter[Int](classOf[Int]) {
      val czCol = czzCol
      def newBuilder(e:Processor#GenElt) = scala.collection.mutable.BitSet.newBuilder
    }
    def apply(p:Type) = a
  }
  object MUnrolledBuffer extends CollectionAdapter[scala.collection.mutable.UnrolledBuffer[_],Processor#GenElt] {
    class Inner[E](p:ParameterizedType) extends SeqAdapter(p.getActualTypeArguments()(0)) {
      val czCol = czzCol
      def newBuilder(e:Processor#GenElt) = new scala.collection.mutable.UnrolledBuffer()(ClassTag(Binder.findClass(czElt)))
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object JBitSetAdapter extends CollectionAdapter[java.util.BitSet,Processor#GenElt] {
    val a = new SeqAdapter[Int](classOf[Int]) {
      val czCol = czzCol
      def newBuilder(e:Processor#GenElt) = new Builder[Int,java.util.BitSet] {
        val tmp = new java.util.BitSet
        def +=(elem: Int):this.type = { tmp.set(elem); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.BitSet = tmp
      }
    }
    def apply(p:Type) = a
  }
  object JEnumSetAdapter extends CollectionAdapter[java.util.EnumSet[_],Processor#GenElt] {
    class Inner[E<:Enum[E]](p:ParameterizedType) extends SeqAdapter(p.getActualTypeArguments()(0)) {
      val czCol = p
      def newBuilder(e:Processor#GenElt) = new Builder[E,java.util.EnumSet[E]] {
        val tmp = java.util.EnumSet.noneOf(czElt.asInstanceOf[Class[E]])
        def +=(elem: E):this.type = { tmp.add(elem); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.EnumSet[E] = tmp
      }
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object JEnumMapAdapter extends CollectionAdapter[java.util.EnumMap[_,_],Processor#GenElt] {
    class Inner[E<:Enum[E],X](p:ParameterizedType) extends MapAdapter(p.getActualTypeArguments()(0),p.getActualTypeArguments()(1)) {
      val czCol = p
      def newBuilder(e:Processor#GenElt) = new Builder[(E,X),java.util.EnumMap[E,X]] {
        val tmp = new java.util.EnumMap[E,X](czKey.asInstanceOf[Class[E]])
        def +=(elem: (E,X)):this.type = { tmp.put(elem._1,elem._2); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.EnumMap[E,X] = tmp
      }
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object JPropertiesAdapter extends CollectionAdapter[java.util.Properties,Processor#GenElt] {
    val a = new MapAdapter[String,String](classOf[String],classOf[String]) {
      val czCol = czzCol
      def newBuilder(e:Processor#GenElt) = new Builder[(String,String),java.util.Properties] {
        val tmp = new java.util.Properties
        def +=(elem: (String,String)):this.type = { tmp.setProperty(elem._1,elem._2); this }
        def clear(): Unit = tmp.clear
        def result(): java.util.Properties = tmp
      }
    }
    def apply(t:Type) = a
  }
  object IntMapAdapter extends CollectionAdapter[scala.collection.immutable.IntMap[_],Processor#GenElt] {
    class Inner[X](p:ParameterizedType) extends MapAdapter[Int,X](classOf[Int],p.getActualTypeArguments()(0)) {
      val czCol = p
      def newBuilder(e:Processor#GenElt) = scala.collection.immutable.IntMap.canBuildFrom[X,X].apply
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object LongMapAdapter extends CollectionAdapter[scala.collection.immutable.LongMap[_],Processor#GenElt] {
    class Inner[X](p:ParameterizedType) extends MapAdapter[Long,X](classOf[Long],p.getActualTypeArguments()(0)) {
      val czCol = p
      def newBuilder(e:Processor#GenElt) = scala.collection.immutable.LongMap.canBuildFrom[X,X].apply
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object HistoryAdapter extends CollectionAdapter[scala.collection.mutable.History[_,_],Processor#GenElt] {
    class Inner[X,Y](p:ParameterizedType) extends SeqAdapter(classOf[Pair[X,Y]]) {
      val czCol = p
      def newBuilder(e:Processor#GenElt) = new Builder[(X,Y),scala.collection.mutable.History[X,Y]] {
        val tmp = new scala.collection.mutable.History[X,Y]
        def +=(elem: (X,Y)):this.type = { tmp.notify(elem._2,elem._1); this }
        def clear(): Unit = tmp.clear
        def result(): scala.collection.mutable.History[X,Y] = tmp
      }
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
  object RevertibleHistoryAdapter extends CollectionAdapter[scala.collection.mutable.RevertibleHistory[_,_],Processor#GenElt] {
    class Inner[X<:scala.collection.mutable.Undoable,Y](p:ParameterizedType) extends SeqAdapter(classOf[Pair[X,Y]]) {
      val czCol = p
      def newBuilder(e:Processor#GenElt) = new Builder[(X,Y),scala.collection.mutable.RevertibleHistory[X,Y]] {
        val tmp = new scala.collection.mutable.RevertibleHistory[X,Y]
        def +=(elem: (X,Y)):this.type = { tmp.notify(elem._2,elem._1); this }
        def clear(): Unit = tmp.clear
        def result(): scala.collection.mutable.RevertibleHistory[X,Y] = tmp
      }
    }
    def apply(t:Type) = t match {
      case p:ParameterizedType => new Inner(p)
    }
  }
    
  /** factory to fill up the Collection adapter map */
  def apply[E<:Processor#GenElt](a:CollectionAdapter[_,E]*):Map[Class[_],CollectionAdapter[_,E]] = {
    val self = HashMap[Class[_],CollectionAdapter[_,E]]()
    for (x<-a) self += x.czzCol -> x
    self.withDefault(cz=>new ReflexiveAdapter(cz))
  }
  
  
  object ReflexiveAdapter {
    def apply(t:Type) = new ReflexiveAdapter(t)(t)
  }
  
  /** Same as above, but defined by reflection where possible.
   *  This is the default when the collection class is not found in the map.
   */
  protected class ReflexiveAdapter(cz:Class[_]) extends CollectionAdapter[Any,Processor#GenElt]()(ClassTag(cz)) { self=>
    //checks if expected is a Map
    
    def apply(t:Type):BaseAdapter[Nothing] = {
      t match {
        case p:ParameterizedType if p.getActualTypeArguments().length==2 =>
          new MapAdapter[Any,Nothing](p.getActualTypeArguments()(0),eltType(p)) {
            val czCol = t
            def newBuilder(e:Processor#GenElt):Builder[Any,Any] = self.newBuilder(e)
          }
        case x =>
          new SeqAdapter[Nothing](eltType(t)) {
            val czCol = t
            def newBuilder(e:Processor#GenElt):Builder[Any,Any] = self.newBuilder(e)
          }
      }
    }

    /** This will be used to build a collection whenever needed */
    protected def newBuilder(e:Processor#GenElt):Builder[Any,Any] = {
      def getJInstance[X]:X = try {
        czzCol.newInstance.asInstanceOf[X]
      } catch {
        case e:InstantiationException => if (czzCol.isInterface || Modifier.isAbstract(czzCol.getModifiers)) throw new IllegalArgumentException(s"Cannot spawn a Java collection defined by an interface or abstract class: $czzCol")
                                         throw e
      }

      if (czzCol.isArray) {
        ArrayBuilder.make()(ClassTag(czzCol.asInstanceOf[Class[Array[_]]].getComponentType))
      } else if (czzCol<classOf[scala.collection.MapLike[_,_,_]] || czzCol<classOf[scala.collection.Traversable[_]]) {
        //try and find canBuildFrom
        //if fails, try and find associated object
        //if found, try and find newBuilder
        //if newBuilder not found, try and find empty, then newBuilder
        (try {                       //try canBuildFrom by reflection
           czzCol.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,Any,_]].apply()
         } catch {
           case e:java.lang.NoSuchMethodException =>
             try {
               var c:RichClass[_] = null
               try {
                 c = Class.forName(czzCol.getName+"$")
                 c.c.getMethod("newBuilder").invoke(c.asObject)
               } catch {
                 case e:java.lang.NoSuchMethodException =>
                   val e = c.c.getMethod("empty").invoke(c.asObject)
                   e.getClass.getMethod("newBuilder").invoke(e)  
               }
            } catch { case e:Throwable=> 
              if (czzCol.isInterface) throw new IllegalArgumentException(s"Cannot spawn a collection defined by an interface: $czzCol")
              if (Modifier.isAbstract(czzCol.getModifiers)) throw new IllegalArgumentException(s"Cannot spawn an abstract collection unless it happens to possess a static canBuildFrom() method : $czzCol")
              throw new IllegalArgumentException(s"Collection $czzCol cannot be spawned as a stand-alone collection : ${e.getMessage}")
            }
        }).asInstanceOf[Builder[Any,Any]]
      } else if (czzCol<classOf[java.util.Map[_,_]]) {
        new Builder[Any,Any] {
          val tmp = getJInstance[java.util.Map[Any,Any]]
          def +=(elem: Any):this.type = {
            elem match {
              case a:Pair[_,_] => tmp.put(a._1,a._2); this
              case _           => throw new IllegalArgumentException(s"a ${classOf[Pair[_,_]]} is expected when filling up a map")        
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
  }
  
  val defaultMap = CollectionAdapter(BitSetAdapter,JBitSetAdapter,JEnumSetAdapter,IntMapAdapter,LongMapAdapter,MBitSetAdapter,MUnrolledBuffer,HistoryAdapter,JEnumMapAdapter,JPropertiesAdapter,RevertibleHistoryAdapter)
}