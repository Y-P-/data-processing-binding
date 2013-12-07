package loader.reflect

import java.lang.reflect.{Field,Method,Type,ParameterizedType,WildcardType,GenericArrayType,TypeVariable}
import loader.core.definition.Def
import loader.core.context.FieldAnnot
import loader.commons._
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
  
/** A Binder ties an AccessibleObject (Field, Method with one argument, which is hidden in a DataActor) with a ConversionSolver and
 *  some conversion data.
 *  As a result, it can be used to automatically set any value into into, possibly automatically converted to the appropriate type.
 *  Binder cannot be created by the user: the Binder factory must be used for that.
 *  The Binder exists only to store the relevant data pertaining to the underlying operations (conversion, field/method setting etc.)
 *  An Instance must be created in order to use the binder on an actual object.
 *  
 *  A Binder is kind of a lazy immutable object: once its internal state is calculated (which happens only once the first
 *  conversion is requested), it never changes.
 *  It is important to note that this implies that a Binder can only convert from one type of end data.
 *  If you have once read a String to set a int field, for example, then you cannot use the same binder to use an int as input.
 *  Another Binder, using the same DataActor would have to be used for that purpose (the internal conversion is obviously not the same!) 
 *  
 *  The Binder works on the expected type set in the DataBinder. Such a type (which is the one declared on the field, method...)
 *  cannot include any Wildcard. Thus, Map[String,Array[Properties]] is acceptable, but not Map[String,Array[_<:Properties]]
 *  
 *  There are only few acceptable sequences of calls to use a Binder properly:
 *  
 *      val b:Binder[_,_] = Binder(fld,solver,fd,false)  //get the binder (not on a collection)
 *      val x:b.I  = b(anObject)                         //bind it to an object
 *      x.set(a,e)                                       //set value a to x
 *
 *      val b:Binder[_,_] = Binder(fld,solver,fd,true)   //get the binder (on a collection)
 *      val x:b.I = b(anObject)                          //bind it to an object
 *      val y:b?I = x.subInstance                        //enter the collection
 *      y.set(a,e0)                                      //set value a to underlying collection in y
 *      y.set(b,e1)                                      //set value b to underlying collection in y
 *      y.set(z,en)                                      //set value z to underlying collection in y
 *      y.close(e)                                       //terminate the collection, which assigns it to fld
 *    
 *    In case the collection is a Map, the set must apply to Assoc objects (i.e. it is then expected that a, b,..., z have the Assoc interface.
 *    At the end of the sequence, anObject.fld is set to the received value, appropriately converted.
 *    
 *  The top class (which cannot be instancied because the constructor is private) is used for binding to a DataActor.
 *  Derived classes (which are also hidden) are used to bind sub-collections. 
 */
sealed class Binder[-E<:Def#Elt] private (val what:DataActor,protected[this] val solver:ConversionSolver[E],protected[this] val fd:AutoConvertData) {
  protected[this] final type I = Analyze#Instance
  private[this] var cached:Analyze = null
  protected[this] def build(on:AnyRef):I = {
    if (cached==null) cached = new Analyze
    cached.newInstance(on,null)
  }
  
  /** The Analyze class is a container that keeps track of the functions used to perform the binding.
   *  These classes are all but invisible to the end user.
   */
  protected class Analyze private[Binder] {              //Binder instance for a pair (object/field or object/method)
    def isCol:Boolean = false                            //indicates whether this is a Collection
    def isMap:Boolean = false                            //indicates whether this is a map
    private[this] var eConvert:(Any, E) => Any = null
    protected[this] def eType:Type = what.expected
    final protected[this] def convert(src:Any,e:E):Any = { //builds the actual value for x as expected from the container
      //finds the converter for source class src; will only be defined on the first invocation (when a value is actually set)
      if (eConvert==null) eConvert=getSolver(src.getClass,eType)
      eConvert(src,e)
    }
   
    def subAnalyze():Analyze = throw new IllegalStateException("sub instance are only allowed on collections")
    protected[reflect] def newInstance(on:AnyRef,parent:I):Instance = new Instance(on)
    
    /** The instance class actually binds an object with a DataActor.
     */
    class Instance protected[Analyze] (on:AnyRef) {
      final def read():Any             = what.get(on)
      def set(x:Any,e:E):Unit          = rcv(convert(x,e),e)
      def close(e:E):Unit              = throw new IllegalStateException("cannot close a field instance")
      def close(key:Any,e:E):Unit      = throw new IllegalStateException("cannot close a field instance")
      def subInstance:I                = throw new IllegalStateException("sub instance are only allowed on collections")
      protected[reflect] def rcv(x:Any,e:E):Unit          = what.set(on,x)
      protected[reflect] def rcv(key:Any,x:Any,e:E):Unit  = throw new IllegalStateException("only a Map instance can receive a (key,value)")
    }
  }

  final def apply(on:AnyRef):I = build(on)
  
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
   
  
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  private class CollectionBinder[-E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData) extends Binder[E](what,solver,fd) {
    private[this] val deepCache = new Array[super.Analyze](6)   //Do we expect deep collection of more than this depth ?
    
    class Analyze(val depth:Int,val parent:super.Analyze) extends super.Analyze {
      override def subAnalyze:Analyze = adapter(eType) match {
        case a if a.isMap => new Map(a,depth+1,this)
        case a            => new Col(a,depth+1,this)
      }
      override protected[reflect] def newInstance(on:AnyRef,parent:I) = new Instance(on,parent)
      class Instance(on:AnyRef,parent:I) extends super.Instance(on) {
        override def subInstance:I = {
          if (deepCache(depth)==null) deepCache(depth) = subAnalyze
          deepCache(depth).newInstance(on,this)
        }
      }
    }
    private class Col(adapt:CollectionAdapter[_,E]#BaseAdapter[_],depth:Int,parent:Analyze) extends Analyze(depth,parent) {
      override final def eType = adapt.czElt
      final override def isCol = true
      final override def isMap = adapt.isMap
      override def newInstance(on:AnyRef,parent:I):Instance = new Instance(on,parent)
      class Instance(on:AnyRef,parent:I) extends super.Instance(on,parent) {
        final var stack:Builder[Any,Any] = _
        private def checkStack(e:E) = if (stack==null) stack = adapt.newBuilder(e).asInstanceOf[Builder[Any,Any]]
        final override def close(e:E):Unit         = { checkStack(e); parent.rcv(stack.result,e) }
        final override def close(key:Any,e:E):Unit = { checkStack(e); parent.rcv(key,stack.result,e) }
        override def rcv(x:Any,e:E):Unit           = { checkStack(e); stack+=x }
      }
    }
    private class Map(adapt:CollectionAdapter[_,E]#BaseAdapter[_],depth:Int,parent:Analyze) extends Col(adapt,depth,parent) {  //used for mapped collection
      final val kType = adapt.asInstanceOf[CollectionAdapter[_,E]#MapAdapter[_,_]].czKey
      protected[this] var kConvert:(Any,E)=>Any = null
      override def newInstance(on:AnyRef,parent:I):Instance = new Instance(on,parent)
      class Instance(on:AnyRef,parent:I) extends super.Instance(on,parent) {
        override def set(x:Any,e:E) = x match {
          case a:Assoc[_,_] => rcv(a.key,convert(a.value,e),e)
          case _ => throw new IllegalStateException(s"a map must receive a ${classOf[Assoc[_,_]]} as data")
        }
        override def rcv(x:Any,e:E) = throw new IllegalStateException()
        override def rcv(key:Any,value:Any,e:E):Unit = {
          if (kConvert==null) kConvert = getSolver(key.getClass,kType)
          super.rcv(kConvert(key,e) -> value,e)
        }
      }
    }
 
    override def build(on:AnyRef):I = new Analyze(0,null).newInstance(on,null)
  }

  /** Factory that builds a Binder with a given DataActor */
  final def apply[E<:Def#Elt](what:DataActor,solver:ConversionSolver[E],fd:AutoConvertData,isCol:Boolean):Binder[E] = {
    if (isCol) new CollectionBinder(what,solver,fd)
    else       new Binder(what,solver,fd)
  }
  
  /** A class that lets use Binders easily to enter values directly.
   *  See the test code for example of use.
   *  - method o is for specifying final inputs
   *  - method u is for specifying layers
   */
  implicit final class Helper(val x: Binder[Def#Elt]#Analyze#Instance) {
    @inline private def sub(f: Helper=>Unit*)                  = { val c:Helper=x.subInstance; f.foreach(_(c)); c.x }
    @inline final def u(f: Helper=>Unit*):Unit                 = sub(f:_*).close(null)
    @inline final def read                                     = x.read()
  }
  object Helper {
    @inline final def o(v:Any*):Helper=>Unit                   = xh=>v.foreach(xh.x.set(_,null))
    @inline final def u(f:Helper=>Unit*):Helper=>Unit          = _.u(f:_*)
    @inline final def u(key:Any)(f:Helper=>Unit*):Helper=>Unit = _.sub(f:_*).close(key,null)
  }
  
}


