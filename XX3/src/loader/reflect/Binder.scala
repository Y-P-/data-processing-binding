package loader.reflect

import java.lang.reflect.{Field,Method,Type,ParameterizedType,WildcardType,GenericArrayType,Constructor,TypeVariable,AccessibleObject,Modifier}
import scala.reflect.ClassTag
import loader.core.definition.Def
import loader.core.context.FieldAnnot
import loader.commons._
import utils.Reflect._
import scala.collection.mutable.Builder

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
 *  The Binder exists only to store the relevant data pertaining to the underlying operations (conversion, field/method setting etc.)
 *  An Instance must be created in order to use the binder on an actual object.
 *  
 *  Note: now Instance is protected, so while the following is still applicable in theory, it cannot be done.
 *    The only available method is apply, which builds an instance (the Binder bound to a given object.)
 *    Instance has only two methods:
 *    - receive(x:AnyRef,e:E), which tells that x was received for that field
 *    - terminate():Unit, which tells that the field is done with (and is only really usefull when building a collection.)
 *    A a consequence, the only available sequence is:
 *  
 *      val b:Binder[_,_] = Binder(fld,solver,fd)
 *      val x:b.Instance  = b.build(anObject,e)
 *      x.receive(a,e)
 *      ..............
 *      x.receive(z,e)
 *      x.terminate
 *    
 *    At the end of the sequence, anObject.fld is set to the received value, appropriately converted.
 *    
 *  New: The correct sequence, which includes the management of simple fields as well as collection (possibly embedded) fields is:
 *  
 *    val b:Binder[_,_] = Binder(fld,solver,fd)
 *    val x:b.Inner = b(o,e1) //using b on object o associated with element e1
 *    b.down()                //entering sub-collection (obviously optional)
 *    b(z,e2)                 //receiving value z from Element e2
 *    b.up()                  //existing sub-collection. exiting from top layer assigns the result (applicable to simple fields)
 *    
 *    So there are always n calls to down and n+1 calls to up
 */

abstract class Binder[-E<:Def#Elt](val what:DataActor) {
  import Binder._
  protected[this] var eType:Type = null                                         //The expected type bound; setting it may be 'complex': it mat be at the bottom of a deep collection
  protected[this] abstract class Instance(on:AnyRef,protected[this] val e:E) {  //Binder instance for a pair (object/field or object/method)
    def container:Instance = null
    def receive(x:Any,e:E)
    def terminate():Unit
    def read():Any = what.get(on)
    def subInstance():Instance = throw new IllegalStateException("sub instance are only allowed on collections")
  }
  /** This class hides layer management and simplifies the API.
   */
  final class Inner protected[Binder] (private[this] var cur:Instance) {
    /** enter a sub-layer. Can be called n times (whatever n) */
    final def down() = cur=cur.subInstance
    /** receives a data value v from element e */
    final def apply(v:Any,e:E) = cur.receive(v,e)
    /** exit a sub-layer or, if at top, assign the value to the field. Must be called exactly n+1 times */
    final def up() = { cur.terminate(); if (cur.container!=null) cur=cur.container }
    /** reads the value. Note that it it an error to use this call if all calls to down have not been unwinded. */
    final def get() = if (cur.container==null) cur.read() else throw new IllegalStateException("You cannot read the value from a sublayer")
  }
  def apply(on:AnyRef,e:E):Inner = new Inner(build(on,e))
  
  protected[this] def build(on:AnyRef,e:E):Instance
  protected[this] val solver:ConversionSolver[E]
  protected[this] def convert(u:Any,e:E):Any
  protected[this] def getFd(src:Class[_]):AutoConvertData
  
  protected[this] final def assign(on:AnyRef,x:Any):Unit = what.set(on, x)      //setting the field/method
  protected[this] final def expected:Type = what.expected
  
  /** This will be used to convert the data element before setting them to a field/method */
  protected[this] final def convertSolver(src:Class[_]):(Any,E)=>Any = {        //finds the converter for source class src; can only be c defined on the first invocation (when a value is actually set)
    val fd= getFd(src)
    solver(src,eType,fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)
  }
  
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
    case t:TypeVariable[_]   => throw new IllegalStateException(s"Real types are expected ; found $t")
    case w:WildcardType      => throw new IllegalStateException(s"Non wilcard types are expected ; found $w")
  }).asInstanceOf[Class[_<:U]]
   

  abstract protected class MultiBinder[-E<:Def#Elt](protected[this] val solver:ConversionSolver[E], protected[this] val fd:Map[Class[_],AutoConvertData], what:DataActor) extends Binder[E](what) {
    protected[this] var convertMap:Map[Class[_],(Any,E)=>Any] = Map.empty  //converting to the actual value
    protected[this] final def convert(u:Any,e:E) = {
      val src = u.getClass
      (convertMap.get(src) match {                                         //fetch the converter to use
        case Some(f) => f
        case None    => val f = convertSolver(src)                         //build it if not already registered
                        convertMap = convertMap + (src->f)
                        f
      })(u,e)                                                              //apply it
    }
    protected[this] final def getFd(src:Class[_]) = fd(src)
  }

  abstract protected class MonoBinder[-E<:Def#Elt](protected[this] val solver:ConversionSolver[E], protected[this] val fd:AutoConvertData, what:DataActor) extends Binder[E](what) {
    protected[this] var convertFct:(Any,E)=>Any = null                        //converting to the actual value
    protected[this] final def convert(u:Any,e:E) = {
      val src = u.getClass
      if (convertFct==null) convertFct = convertSolver(src)
      convertFct(u,e)
    }
    protected[this] final def getFd(src:Class[_]) = fd
  }
  
  /** Binder for a simple (i.e. not a collection) element.
   *  An element could happen to be a collection, but it is treated as a whole (i.e. if conversion occurs, it occurs on the collection itself.)
   */
  protected trait SimpleBinder[-E<:Def#Elt] extends Binder[E] {
    eType = expected
    final protected[this] class Instance(on:AnyRef,e:E) extends super.Instance(on,e) { //Binder instance for a pair (object/field or object/method)
      final def receive(x:Any,e:E) = assign(on,convert(x,e))
      final def terminate():Unit = ()
    }
    protected[this] def build(on:AnyRef,e:E) = new Instance(on,e)
  }
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  protected trait CollectionBinder[-E<:Def#Elt] extends Binder[E] {
    val SZ = 6                                                                          //Do we expect deep collection of more than this depth ?
    protected[this] var depth    = -1
    protected[this] val eTypes   = new Array[Type](SZ)                                  //store calculated values to avoid recomputing them at each step
    protected[this] val kConvert = new Array[(Any,E)=>Any](SZ)                          //store calculated key converters to avoid recomputing them at each step
    protected[this] class Instance(on:AnyRef,val adapt:CollectionAdapter[_,E]#BaseAdapter[_],depth:Int,e:E) extends super.Instance(on,e) { //Binder instance for a pair (object/field or object/method)
      protected[this] val eType = { if (eTypes(depth)==null) eTypes(depth)=adapt.czElt; eTypes(depth) }
      if (CollectionBinder.this.depth<depth) {
        CollectionBinder.this.depth = depth
        CollectionBinder.this.eType = eType            //each level of instance (for deep collections) sets this value (once only till bottom is reached)
      }
      final protected[this] val stack = adapt.newBuilder(e).asInstanceOf[Builder[Any,Any]]
      def terminate():Unit = assign(on,stack.result)
      def receive(x:Any,e:E) = stack+=convert(x,e)
      trait SubInstance extends Instance {             //Instance for deep collections (collections of collections of ...)
        override def container = Instance.this
        override def terminate():Unit = Instance.this.stack+=stack.result //adapt.buildCollection(stack,e)
      }
      final override def subInstance = if (adapt.isMap) new MappedInstance(null,adapter(eType),depth+1,e) with SubInstance else new Instance(null,adapter(eType),depth+1,e) with SubInstance
    }
    protected[this] class MappedInstance(on:AnyRef,adapt:CollectionAdapter[_,E]#BaseAdapter[_],depth:Int,e:E) extends Instance(on,adapt,depth,e) {  //used for mapped collection
      protected[this] val kType = adapt.asInstanceOf[CollectionAdapter[_,E]#MapAdapter[_,_]].czKey
      override def receive(x:Any,e:E) = x match {
        case a:Assoc[_,_] => if (kConvert(depth)==null) kConvert(depth) = solver(a.key.getClass,kType,ConvertData.empty,null).fold(s=>throw new IllegalStateException(s), identity)
                             stack += kConvert(depth)(a.key,e) -> convert(a.value,e)
        case _ => throw new IllegalStateException(s"a map must receive a ${classOf[Assoc[_,_]]} as data")
      }      
    }
    def adapter(t:Type) = solver.collectionSolver(t)(t)
    protected[this] def build(on:AnyRef,e:E) = {
      val adapt = adapter(expected)
      if (adapt.isMap) new MappedInstance(on,adapt,0,e) else new Instance(on,adapt,0,e)
    }
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
  
  
  
  
  implicit protected[this] def toRichClass(t:Type):RichClass[_] = findClass(t)
}
