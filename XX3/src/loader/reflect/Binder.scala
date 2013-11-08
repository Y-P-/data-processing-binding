package loader.reflect

import java.lang.reflect.{Field,Method,Type,ParameterizedType,WildcardType,GenericArrayType,Constructor,TypeVariable,AccessibleObject}
import scala.reflect.ClassTag
import loader.core.definition.Def
import loader.core.context.FieldAnnot
import loader.Assoc

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
    def receive(x:AnyRef,e:E)
    def terminate():Unit
    def read():Any = what.get(on)
  }
  def apply(on:AnyRef):Instance
  protected[this] val solver:ConversionSolver[E]
  protected[this] def convert(u:AnyRef,e:E):Any
  protected[this] final def assign(on:AnyRef,x:Any):Unit = what.set(on, x)      //setting the field/method
  protected[this] final def expected:Type = what.expected
  protected[this] def convertSolver(src:Class[_]):(AnyRef,E)=>Any               //finds the converter for source class src
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
    protected[this] var convertMap:Map[Class[_],(AnyRef,E)=>Any] = Map.empty  //converting to the actual value
    protected[this] final def convert(u:AnyRef,e:E) = {
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
    protected[this] var convertFct:(AnyRef,E)=>Any = null                     //converting to the actual value
    protected[this] final def convert(u:AnyRef,e:E) = {
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
      final def receive(x:AnyRef,e:E) = assign(on,convert(x,e))
      final def terminate():Unit = ()
    }
    protected[this] def convertSolver(src:Class[_]):(AnyRef,E)=>Any = {
      val fd= getFd(src)
      solver(src,expected,fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)
    }
    def apply(on:AnyRef) = new Instance(on)
  }
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  protected trait CollectionBinder[-E<:Def#Elt] extends Binder[E] {
    final class Instance(on:AnyRef) extends super.Instance(on) { //Binder instance for a pair (object/field or object/method)
      val stack = new Stack(eltType(expected),expected)
      final def receive(x:AnyRef,e:E) = stack+=(convert(x,e))
      final def terminate():Unit = assign(on,stack.buildCollection)
    }
    protected[this] def convertSolver(src:Class[_]):(AnyRef,E)=>Any = {
      val fd= getFd(src)
      solver(src,eltType(expected),fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)
    }
    def apply(on:AnyRef) = new Instance(on)
  }
  /** Factory where multiple conversions from various types are expected */
  final def apply[E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:Map[Class[_],AutoConvertData],isCol:Boolean):Binder[E] = {
    if (isCol) new MultiBinder(solver,fd,what) with CollectionBinder[E]
    else       new MultiBinder(solver,fd,what) with SimpleBinder[E]
  }
  /** Factory where only one conversion is expected */
  final def apply[E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData,isCol:Boolean):Binder[E] = {
    if (isCol) new MonoBinder(solver,fd,what) with CollectionBinder[E]
    else       new MonoBinder(solver,fd,what) with SimpleBinder[E]
  }
  
  //TODO ?: improve perf by moving reflection out. Not worth it though (collection building doesn't happen that often).
  class Stack[+X](cx:Class[X],czColl:Class[_],sz:Int=200) extends utils.Stack(cx,sz) {
    def buildCollection:AnyRef = {
      val r = if (czColl.isArray) {
        val r = java.lang.reflect.Array.newInstance(czColl.asInstanceOf[Class[Array[_]]].getComponentType, length).asInstanceOf[Array[X]]
        arrayCopy(r)
        r
      } else if (classOf[scala.collection.MapLike[_,_,_]].isAssignableFrom(czColl)) {
        val builder = czColl.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,Any,_]].apply
        builder.sizeHint(length)
        for (x <- this) builder += { val a=x.asInstanceOf[Assoc[Any,Any]]; (a.key,a.value) }
        builder.result.asInstanceOf[AnyRef]
      } else if (classOf[scala.collection.Iterable[_]].isAssignableFrom(czColl)) {
        val builder = czColl.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,X,_]].apply
        builder.sizeHint(length)
        for (x <- this) builder += x
        builder.result.asInstanceOf[AnyRef]
      } else if (classOf[java.util.Map[_,_]].isAssignableFrom(czColl)) {
        val r = czColl.newInstance.asInstanceOf[java.util.Map[Any,Any]]
        for (x <- this) { val a=x.asInstanceOf[Assoc[Any,Any]]; r.put(a.key,a.value); }
        r
      } else if (classOf[java.util.Collection[_]].isAssignableFrom(czColl)) {
        val r = czColl.newInstance.asInstanceOf[java.util.Collection[X]]
        for (x <- this) r.add(x)
        r
      }
      else
        throw new IllegalStateException("Unsupported container class")
      clear
      r
    }
  }  
}


