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

abstract class Binder[-E<:Def#Elt](val what:DataActor,protected[this] val solver:ConversionSolver[E],protected[this] val fd:AutoConvertData) {
  abstract class Instance(on:AnyRef,protected[this] val e:E) {  //Binder instance for a pair (object/field or object/method)
    def isCol:Boolean                               //indicates whether this is a Collection
    def isMap:Boolean                               //indicates whether this is a map
    def convert(x:Any):Any                          //build the actual value for x as expected from the container
    def close():Unit                                //terminates a seq collection ; can apply to maps iff the seq elements were assocs
    protected def assign(x:Any):Unit                //an internal method to assign the result x to the container
    
    def subInstance():Instance = throw new IllegalStateException("sub instance are only allowed on collections")   //creates a sub instance for encapsulated collections
    def close(key:Any):Unit = throw new IllegalStateException("this method cannot be invoked from a top Instance") //terminates a sub-collection by assigning a key ; applicable if container is a map
    
    final def read():Any = what.get(on)
    final def assign(x:Any,doConvert:Boolean):Unit = assign(if (doConvert) convert(x) else x)
    final def set(x:Any) = x match {
      case a:Assoc[_,_] => assign(a,false)
      case x            => assign(x,true)
    }
  }

  def apply(on:AnyRef,e:E):Instance = build(on,e)
  
  protected[this] def build(on:AnyRef,e:E):Instance  
  protected[this] final def adapter(t:Type) = solver.collectionSolver(Binder.findClass(t))(t)
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
   
  
  /** Binder for a simple (i.e. not a collection) element.
   *  An element could happen to be a collection, but it is treated as a whole (i.e. if conversion occurs, it occurs on the collection itself.)
   */
  protected class SimpleBinder[-E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData) extends Binder[E](what,solver,fd) {
    protected[this] class Instance(on:AnyRef,e:E) extends super.Instance(on,e) { //Binder instance for a pair (object/field or object/method)
      protected[this] def eType = what.expected
      def isMap = false
      def isCol = false
      protected def assign(x:Any) = what.set(on,convert(x))
      def close():Unit = throw new IllegalStateException("cannot close a field instance")
      final def convert(src:Any):Any = {        //finds the converter for source class src; can only be c defined on the first invocation (when a value is actually set)
        solver(src.getClass,eType,fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)(src,e)
      }
    }
    protected[this] def build(on:AnyRef,e:E):super.Instance = new Instance(on,e)
  }
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  protected class CollectionBinder[-E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData) extends SimpleBinder[E](what,solver,fd) {
    val SZ = 6                                                          //Do we expect deep collection of more than this depth ?
    //eType = null
    //protected[this] var depth    = -1
    //protected[this] val eTypes   = new Array[Type](SZ)                  //store calculated values to avoid recomputing them at each step
    //protected[this] val kConvert = new Array[(Any,E)=>Any](SZ)          //store calculated key converters to avoid recomputing them at each step
    
    private[this] class Instance(on:AnyRef,e:E) extends super.Instance(on,e) {
      final def binder:CollectionBinder.this.type = CollectionBinder.this
      override def subInstance = adapter(eType) match {
        case a if a.isMap => new Map(a)
        case a            => new Col(a)
      }
      protected[this] class Col(adapt:CollectionAdapter[_,E]#BaseAdapter[_]) extends Instance(on,e) {
        final def parent = Instance.this
        final val depth:Int = if (parent.isInstanceOf[Col]) parent.asInstanceOf[Col].depth+1 else 0
        final val stack = adapt.newBuilder(e).asInstanceOf[Builder[Any,Any]]
        protected[this] override final val eType = adapt.czElt
        final override def isCol = true
        final override def isMap = adapt.isMap
        final override def close():Unit         = parent.assign(stack.result,false)
        final override def close(key:Any):Unit  = if (!parent.isMap) throw new IllegalStateException("this method can only be invoked if the containing instance is a map")
                                                  else parent.assign(key-->stack.result,false)
        override protected def assign(x:Any):Unit = stack += x
      }
      protected[this] class Map(adapt:CollectionAdapter[_,E]#BaseAdapter[_]) extends Col(adapt) {  //used for mapped collection
        protected[this] final val kType = adapt.asInstanceOf[CollectionAdapter[_,E]#MapAdapter[_,_]].czKey
        private[this] var kConvert:(Any,E)=>Any =  null
        override protected def assign(x:Any) = x match {
          case a:Assoc[_,_] => if (kConvert==null) kConvert = solver(a.key.getClass,kType,ConvertData.empty,null).fold(s=>throw new IllegalStateException(s), identity)
                               super.assign(kConvert(a.key,e) -> convert(a.value))
          case _ => throw new IllegalStateException(s"a map must receive a ${classOf[Assoc[_,_]]} as data")
        }
      }
    }
 
    protected[this] override def build(on:AnyRef,e:E) = new Instance(on,e).subInstance
  }

  def fd(check0:String,param0:String,valid0:String,convert0:String) = new AutoConvertData {
    def valid: String = valid0
    def check: String = check0
    def param: String = param0
    def convert: String = convert0
  }
  /** Factory */
  final def apply[E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:Map[Class[_],AutoConvertData],isCol:Boolean):Binder[E] = {
    if (isCol) new CollectionBinder(what,solver,Binder.fd("","","",""))
    else       new SimpleBinder(what,solver,Binder.fd("","","",""))
  }
  
  implicit protected[this] def toRichClass(t:Type):RichClass[_] = findClass(t)
}

