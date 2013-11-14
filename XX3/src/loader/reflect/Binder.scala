package loader.reflect

import java.lang.reflect.{Field,Method,Type,ParameterizedType,WildcardType,GenericArrayType,Constructor,TypeVariable,AccessibleObject,Modifier}
import scala.reflect.ClassTag
import loader.core.definition.Def
import loader.core.context.FieldAnnot
import loader.commons._

trait AutoConvertData extends ConvertData {
  def convert:String
}
object AutoConvertData {
  implicit def apply(fd:FieldAnnot) = new AutoConvertData {
    val convert = fd.convert
    val param   = fd.param
    val check   = fd.check
    val valid   = fd.valid
  }
}
  
/** A Binder ties an AccessibleObject (Field, Method with one argument) with a ConversionSolver and some conversion data.
 *  As a result, it can be used to automatically set any value into into, possibly automatically converted to the appropriate type.
 *  Binder cannot be created by the user: the Binder factory must be used for that.
 *  The only available method is apply, which builds an instance (the Binder bound to a given object.)
 *  Instance has only two methods:
 *  - receive(x:AnyRef,e:E), which tells that x was received for that field
 *  - terminate():Unit, which tells that the field is done with (and is only really usefull when building a collection.)
 *  A a consequence, the only available sequence is:
 *  
 *    val b:Binder[_,_] = Binder(fld,solver,fd)
 *    val x:b.Instance  = b(anObject)
 *    x.receive(a,e)
 *    ..............
 *    x.receive(z,e)
 *    x.terminate
 *    
 *  At the end of the sequence, anObject.fld is set to the received value, appropriately converted.
 */

abstract class Binder[-E<:Def#Elt](val what:DataActor) {
  abstract class Instance(on:AnyRef) {                                          //Binder instance for a pair (object/field or object/method)
    def container:Instance
    def receive(x:Any,e:E)
    def terminate():Unit
    def read():Any = what.get(on)
    def subInstance():Instance = throw new IllegalStateException("sub instance are only allowed on collections")
  }
  def apply(on:AnyRef):Instance
  protected[this] val solver:ConversionSolver[E]
  protected[this] def convert(u:Any,e:E):Any
  protected[this] final def assign(on:AnyRef,x:Any):Unit = what.set(on, x)      //setting the field/method
  protected[this] final def expected:Type = what.expected
  protected[this] def convertSolver(src:Class[_]):(Any,E)=>Any                  //finds the converter for source class src
  protected[this] def getFd(src:Class[_]):AutoConvertData
}

object Binder {
  /** underlying class for a given type.
   *  Class             => that object
   *  GenericArrayType  => the underlying array class (stripped of genericity)
   *  ParameterizedType => the underlying class (stripped of genericity)
   *  TypeVariable      => is unexpected
   *  WildcardType      => is unexpected
   */
  implicit def findClass[U](gType:Type):Class[_<:U] = (gType match {  //exhaustive check
    case c:Class[_]          => c
    case g:GenericArrayType  => java.lang.reflect.Array.newInstance(g.getGenericComponentType,0).getClass
    case p:ParameterizedType => p.getRawType
    case t:TypeVariable[_]   => throw new IllegalStateException
    case w:WildcardType      => throw new IllegalStateException  //w.getUpperBounds().find(_ match { case c:Class[_] => !c.isInterface; case _ => false }).getOrElse(null).asInstanceOf[Class[_]]
  }).asInstanceOf[Class[_<:U]]
   
  /** Type of the underlying contained element.
   *  can only be called when t is associated with a container of some sort.
   */
  final protected def eltType(t:Type):Type = t match {
    case p:ParameterizedType     => val args = p.getActualTypeArguments; args(args.length-1)
    case g:GenericArrayType      => g.getGenericComponentType
    case a:Class[_] if a.isArray => a.getComponentType
    case _                       => throw new IllegalStateException
  }

  abstract protected class MultiBinder[-E<:Def#Elt](protected[this] val solver:ConversionSolver[E], protected[this] val fd:Map[Class[_],AutoConvertData], what:DataActor) extends Binder[E](what) {
    protected[this] var convertMap:Map[Class[_],(Any,E)=>Any] = Map.empty  //converting to the actual value
    protected[this] final def convert(u:Any,e:E) = {
      val src = u.getClass
      (convertMap.get(src) match {                                            //fetch the converter to use
        case Some(f) => f
        case None    => val f = convertSolver(src)                            //build it if not already registered
                        convertMap = convertMap + (src->f)
                        f
      })(u,e)                                                                 //apply it
    }
    protected[this] final def getFd(src:Class[_]) = fd(src)
  }

  abstract protected class MonoBinder[-E<:Def#Elt](protected[this] val solver:ConversionSolver[E], protected[this] val fd:AutoConvertData, what:DataActor) extends Binder[E](what) {
    protected[this] var convertFct:(Any,E)=>Any = null                     //converting to the actual value
    protected[this] final def convert(u:Any,e:E) = {
      val src = u.getClass
      if (convertFct==null) convertFct = convertSolver(src)
      convertFct(u,e)
    }
    protected[this] final def getFd(src:Class[_]) = fd
  }
  
  /** Binder for a simple (i.e. not a collection) element.
   *  An element could happen to be a collection, but it is treated as a whole (i.e. if conversion occurs, it occurs on the collection itself)
   */
  protected trait SimpleBinder[-E<:Def#Elt] extends Binder[E] {
    final class Instance(on:AnyRef) extends super.Instance(on) { //Binder instance for a pair (object/field or object/method)
      final def container = null
      final def receive(x:Any,e:E) = assign(on,convert(x,e))
      final def terminate():Unit = ()
    }
    protected[this] def convertSolver(src:Class[_]):(Any,E)=>Any = {
      val fd= getFd(src)
      solver(src,expected,fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)
    }
    def apply(on:AnyRef) = new Instance(on)
  }
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  protected trait CollectionBinder[-E<:Def#Elt] extends Binder[E] {
    protected[this] var eType:Type = null
    class Instance(on:AnyRef,colClzz:Type) extends super.Instance(on) { //Binder instance for a pair (object/field or object/method)
      def container:Instance = null
      eType = eltType(colClzz)
      def buildStack:Stack[_] = new Stack(eType,colClzz)
      val stack = buildStack
      def receive(x:Any,e:E) = stack+=convert(x,e)
      def terminate():Unit = assign(on,stack.buildCollection)
      final override def subInstance = new Instance(null,eltType(colClzz)) { //Instance for deep collections (collections of collections of ...)
        override def container = Instance.this
        override def terminate():Unit = Instance.this.stack+=stack.buildCollection
      }
    }
    protected[this] def convertSolver(src:Class[_]):(Any,E)=>Any = {
      val fd= getFd(src)
      solver(src,eType,fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)
    }
    def apply(on:AnyRef) = new Instance(on,expected)
  }
  /** Binder for a map collection. It behaves roughly as a collection, but:
   *  - Assoc are expected as input data.
   *  - the key may have to be converted. In that case, only basic conversions (no ConvertData, no choice by name) are available.
   */
  protected trait MapBinder[-E<:Def#Elt] extends CollectionBinder[E] {
    protected[this] var keyConvert:(Any,E)=>Any = null
    class Instance(on:AnyRef,colClzz:Type) extends super.Instance(on,colClzz) { //Binder instance for a pair (object/field or object/method)
      val kType = try { //type for key is first param in the list
        val p=colClzz.asInstanceOf[ParameterizedType].getActualTypeArguments
        if (p.length<=1) throw new IllegalStateException(s"class $colClzz has not at least 2 parameters and cannot be used as a spawned map")
        if (p(0)==classOf[AnyRef]) throw new IllegalStateException(s"class ${p(0)} is too general for a map key")
        p(0)
      } catch {
        case e:Exception => throw new IllegalStateException(s"class $colClzz cannot be used as a spawned map: $e")
      }
      override def buildStack = new Stack(classOf[Assoc[_,_]],colClzz)
      override def receive(x:Any,e:E) = x match {
        case a:Assoc[_,_] => if (keyConvert==null) keyConvert = solver(a.key.getClass,kType,ConvertData.empty,null).fold(s=>throw new IllegalStateException(s), identity)
                             stack += keyConvert(a.key,e) --> convert(a.value,e)
        case _ => throw new IllegalStateException(s"a map must receive a ${classOf[Assoc[_,_]]} as data")
      }
    }
    override def apply(on:AnyRef) = new Instance(on,expected)
  }
  /** Factory where multiple conversions from various types are expected */
  final def apply[E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:Map[Class[_],AutoConvertData],isCol:Boolean):Binder[E] = {
    if (isCol) if (what.expected.getTypeParameters.length>1) new MultiBinder(solver,fd,what) with MapBinder[E]
               else                                          new MultiBinder(solver,fd,what) with CollectionBinder[E]
    else                                                     new MultiBinder(solver,fd,what) with SimpleBinder[E]
  }
  /** Factory where only one conversion is expected */
  final def apply[E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData,isCol:Boolean):Binder[E] = {
    if (isCol) if (what.expected.getTypeParameters.length>1) new MonoBinder(solver,fd,what) with MapBinder[E]
               else                                          new MonoBinder(solver,fd,what) with CollectionBinder[E]
    else                                                     new MonoBinder(solver,fd,what) with SimpleBinder[E]
  }
  
  //TODO ?: improve perf by moving reflection out. Not worth it though (collection building doesn't happen that often).
  class Stack[+X](cx:Class[X],czColl:Class[_],sz:Int=200) extends utils.Stack(cx,sz) {
    protected[this] lazy val mkBuilder = try {
      czColl.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,Any,_]]
    } catch {
      case e:NoSuchMethodException => if (czColl.isInterface) throw new IllegalStateException(s"Cannot spawn a collection defined by an interface: $czColl")
                                      if (Modifier.isAbstract(czColl.getModifiers)) throw new IllegalStateException(s"Cannot spawn an abstract collection unless it happens to possess a static canBuildFrom() method : $czColl")
                                      throw e
    }
    protected[this] def getBuilder = { val builder = mkBuilder.apply; builder.sizeHint(length); builder }
    protected[this] def getJInstance[X]:X = try {
      czColl.newInstance.asInstanceOf[X]
    } catch {
      case e:InstantiationException => if (czColl.isInterface || Modifier.isAbstract(czColl.getModifiers)) throw new IllegalStateException(s"Cannot spawn a Java collection defined by an interface or abstract class: $czColl")
                                       throw e
    }
    def buildCollection:AnyRef = {
      val r = if (czColl.isArray) {
        val r = java.lang.reflect.Array.newInstance(czColl.asInstanceOf[Class[Array[_]]].getComponentType, length).asInstanceOf[Array[X]]
        arrayCopy(r)
        r
      } else if (classOf[scala.collection.MapLike[_,_,_]].isAssignableFrom(czColl)) {
        val builder = getBuilder
        for (x <- this) builder += { val a=x.asInstanceOf[Assoc[Any,Any]]; (a.key,a.value) }
        builder.result.asInstanceOf[AnyRef]
      } else if (classOf[scala.collection.Iterable[_]].isAssignableFrom(czColl)) {
        val builder = getBuilder
        for (x <- this) builder += x
        builder.result.asInstanceOf[AnyRef]
      } else if (classOf[java.util.Map[_,_]].isAssignableFrom(czColl)) {
        val r = getJInstance[java.util.Map[Any,Any]]
        for (x <- this) x match {
          case a:Assoc[_,_] => r.put(a.key,a.value)
          case _            => throw new IllegalArgumentException(s"an ${classOf[Assoc[_,_]]} is expected when filling up a map")
        }
        r
      } else if (classOf[java.util.Collection[_]].isAssignableFrom(czColl)) {
        val r = getJInstance[java.util.Collection[X]]
        for (x <- this) r.add(x)
        r
      } else
        throw new IllegalStateException("Unsupported container class")
      clear
      r
    }
  }  
}


