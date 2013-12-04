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
  abstract class Analyze(on:AnyRef,protected[this] val e:E) {  //Binder instance for a pair (object/field or object/method)
    final def binder:Binder.this.type = Binder.this
    def isCol:Boolean                               //indicates whether this is a Collection
    def isMap:Boolean                               //indicates whether this is a map
    def convert(x:Any):Any                          //builds the actual value for x as expected from the container
    def close():Unit                                //terminates a seq collection ; can apply to maps iff the seq elements were assocs
    
    def subAnalyze():Analyze = throw new IllegalStateException("sub instance are only allowed on collections")   //creates a sub instance for encapsulated collections
    def close(key:Any):Unit  = throw new IllegalStateException("this method cannot be invoked from a top Instance") //terminates a sub-collection by assigning a key ; applicable if container is a map
    
    final def read():Any = what.get(on)
    def set(x:Any):Unit = rcv(convert(x))
    def rcv(x:Any):Unit
    def rcv(key:Any,x:Any):Unit = throw new IllegalStateException("only a Map instance can receive a key")
  }

  def apply(on:AnyRef,e:E):Analyze = build(on,e)
  
  protected[this] def build(on:AnyRef,e:E):Analyze  
  protected[this] final def adapter(t:Type) = solver.collectionSolver(Binder.findClass(t))(t)
  protected[this] final def getSolver(cz:Class[_],t:Type) = solver(cz,Binder.findClass(t),fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)

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
    protected[this] class Analyze(on:AnyRef,e:E) extends super.Analyze(on,e) { //Binder instance for a pair (object/field or object/method)
      protected[this] def eType = what.expected
      def isMap = false
      def isCol = false
      def rcv(x:Any) = what.set(on,x)
      def close():Unit = throw new IllegalStateException("cannot close a field instance")
      final def convert(src:Any):Any = {        //finds the converter for source class src; can only be c defined on the first invocation (when a value is actually set)
        getSolver(src.getClass,eType)(src,e)
      }
    }
    protected[this] def build(on:AnyRef,e:E):super.Analyze = new Analyze(on,e)
  }
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  protected class CollectionBinder[-E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData) extends SimpleBinder[E](what,solver,fd) {
    //val SZ = 6                                                          //Do we expect deep collection of more than this depth ?
    //eType = null
    //protected[this] var depth    = -1
    //protected[this] val eTypes   = new Array[Type](SZ)                  //store calculated values to avoid recomputing them at each step
    //protected[this] val kConvert = new Array[(Any,E)=>Any](SZ)          //store calculated key converters to avoid recomputing them at each step
    
    protected[this] class Analyze(on:AnyRef,e:E,val depth:Int,val parent:Analyze) extends super.Analyze(on,e) {
      override def subAnalyze = adapter(eType) match {
        case a if a.isMap => new Map(a,depth+1,this)
        case a            => new Col(a,depth+1,this)
      }
    }
    protected[this] class Col(adapt:CollectionAdapter[_,E]#BaseAdapter[_],depth:Int,parent:Analyze) extends Analyze(null,null.asInstanceOf[E],depth,parent) {
      final val stack = adapt.newBuilder(e).asInstanceOf[Builder[Any,Any]]
      protected[this] override final val eType = adapt.czElt
      final override def isCol               = true
      final override def isMap               = adapt.isMap
      final override def close():Unit        = parent.rcv(stack.result)
      final override def close(key:Any):Unit = parent.rcv(key,stack.result)
      override def rcv(x:Any):Unit           = stack += x
    }
    protected[this] class Map(adapt:CollectionAdapter[_,E]#BaseAdapter[_],depth:Int,parent:Analyze) extends Col(adapt,depth,parent) {  //used for mapped collection
      protected[this] final val kType = adapt.asInstanceOf[CollectionAdapter[_,E]#MapAdapter[_,_]].czKey
      private[this] var kConvert:(Any,E)=>Any =  null
      override def set(x:Any) = x match {
        case a:Assoc[_,_] => rcv(a.key,convert(a.value))
        case _ => throw new IllegalStateException(s"a map must receive a ${classOf[Assoc[_,_]]} as data")
      }
      override def rcv(x:Any) = throw new IllegalStateException(s"a map cannot receive a single value: $x")
      override def rcv(key:Any,value:Any):Unit = {
        if (kConvert==null) kConvert = getSolver(key.getClass,kType)
        super.rcv(kConvert(key,e) -> value)
      }
    }
 
    protected[this] override def build(on:AnyRef,e:E) = new Analyze(on,e,0,null)
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

