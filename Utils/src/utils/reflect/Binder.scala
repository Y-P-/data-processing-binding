package utils.reflect

import java.lang.reflect.{Field,Method,Type,ParameterizedType,WildcardType,GenericArrayType,TypeVariable}
import scala.collection.mutable.Builder
import Reflect.findClass

trait AutoConvertData extends ConvertData {
  def convert:String
}
  
/** A Binder ties an AccessibleObject (Field, Method with one argument, which is hidden in a DataActor) with a ConversionSolver and
 *  some conversion data.
 *  As a result, it can be used to automatically set any value, possibly automatically converted to the appropriate type.
 *  Binder cannot be created by the user: the Binder factory must be used for that.
 *  The Binder exists only to store the relevant data pertaining to the underlying operations (conversion, field/method setting etc.)
 *  It can be kept and reused.
 *  An Instance must be created in order to use the binder on an actual object.
 *  
 *  A Binder is kind of a lazy immutable object: once its internal state is calculated (which happens only once the first
 *  conversion is requested), it never changes.
 *  It is important to note that this implies that a Binder can only convert from one type of data.
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
sealed class Binder private (val what:DataActor,protected[this] val solver:ConversionSolver,protected[this] val fd:AutoConvertData) {
  final type I = Analyze#Instance
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
    private[this] var eConvert:Any => Any = null
    protected[this] def eType:Type = what.expected
    val eClass:Class[_] = findClass(eType)
    final protected[this] def convert(src:Any):Any = { //builds the actual value for x as expected from the container
      //finds the converter for source class src; will only be defined on the first invocation (when a value is actually set)
      if (eConvert==null) eConvert=getSolver(src.getClass,eType)
      eConvert(src)
    }
   
    def subAnalyze():Analyze = throw new IllegalStateException("sub instance are only allowed on collections")
    protected[reflect] def newInstance(on:AnyRef,parent:I):Instance = new Instance(on)
    
    /** The instance class actually binds an object with a DataActor.
     */
    class Instance protected[Analyze] (val on:AnyRef) {
      final def binder            = Binder.this
      final def read():Any        = what.get(on)
      def eltClass                = Analyze.this.eClass
      def endClass                = Analyze.this.eType
      def set(x:Any):Unit         = rcv(convert(x))
      def asT:Traversable[Any]    = throw new IllegalStateException("cannot cast a field instance as a Traversable")
      def close():Unit            = throw new IllegalStateException("cannot close a field instance")
      def close(key:Any):Unit     = throw new IllegalStateException("cannot close a field instance")
      def subInstance:I           = throw new IllegalStateException("sub instance are only allowed on collections")
      //use with care: this is direct access to the underlying collection WITHOUT conversion
      def rcv(x:Any):Unit         = what.set(on,x)
      def rcv(key:Any,x:Any):Unit = throw new IllegalStateException("only a Map instance can receive a (key,value)")
    }
  }

  final def apply(on:AnyRef):I = build(on)
  
  protected[this] final def getSolver(cz:Class[_],t:Type) = solver(cz,findClass(t),fd,fd.convert).fold(s=>throw new IllegalStateException(s), identity)

}

object Binder {
  
  trait Assoc[+K,+T] {
    def key:K
    def value:T
    final override def toString = "Assoc("+key+" -> "+value+")"
  }   
  /** Used to map a pair key/value to store maps.
   *  Use this as the return type of an end method for items stored as maps.
   */
  final case class AssocElt[+K,+T](final val key:K, final val value:T) extends Assoc[K,T] {
    final protected def this() = this(null.asInstanceOf[K],null.asInstanceOf[T])
  }
  /** Use --> instead of -> for building Assoc as a shortcut
   */
  final implicit class AssocLeft[K](final val key:K) {
    @inline def -->[T] (t:T) = new AssocElt(key,t)
  }
  
  /** Binder for a collection element. It can not be assigned until all elements have been first collected.
   *  Furthermore, the conversion process occurs on the elements themselves, not the container.
   */
  private class CollectionBinder(what:DataActor,solver:ConversionSolver,fd:AutoConvertData) extends Binder(what,solver,fd) {
    private[this] val deepCache = new Array[super.Analyze](6)   //Do we expect deep collection of more than this depth ?
    
    class Analyze(val depth:Int,val parent:super.Analyze) extends super.Analyze {
      override def subAnalyze:Analyze = solver.collectionSolver(eType) match {
        case None         => throw new IllegalArgumentException(s"type $eType cannot be identified as a workable collection")
        case Some(a) if a.isMap => new Map(a,depth+1,this)
        case Some(a)            => new Col(a,depth+1,this)
      }
      override protected[reflect] def newInstance(on:AnyRef,parent:I) = new Instance(on,parent)
      class Instance(on:AnyRef,parent:I) extends super.Instance(on) {
        override def subInstance:I = {
          if (deepCache(depth)==null) deepCache(depth) = subAnalyze
          deepCache(depth).newInstance(on,this)
        }
      }
    }
    private class Col(adapt:CollectionAdapter.Adapt,depth:Int,parent:Analyze) extends Analyze(depth,parent) {
      override final def eType = adapt.czElt
      final override def isCol = true
      final override def isMap = adapt.isMap
      override def newInstance(on:AnyRef,parent:I):Instance = new Instance(on,parent)
      class Instance(on:AnyRef,parent:I) extends super.Instance(on,parent) {
        final val stack:Builder[Any,Any]       = adapt.newBuilder.asInstanceOf[Builder[Any,Any]]
        final override def close():Unit        = parent.rcv(stack.result)
        final override def close(key:Any):Unit = parent.rcv(key,stack.result)
        override def rcv(x:Any):Unit           = stack+=x
        override def asT                       = { val r=read(); if (r==null) null else adapt.asTraversable(r.asInstanceOf[adapt.C]) }
      }
    }
    private class Map(adapt:CollectionAdapter.Adapt,depth:Int,parent:Analyze) extends Col(adapt,depth,parent) {  //used for mapped collection
      final val kType = adapt.asInstanceOf[CollectionAdapter[_]#MapAdapter[_,_]].czKey
      protected[this] var kConvert:(Any)=>Any = null
      override def newInstance(on:AnyRef,parent:I):Instance = new Instance(on,parent)
      class Instance(on:AnyRef,parent:I) extends super.Instance(on,parent) {
        override def set(x:Any) = x match {
          case a:Assoc[_,_] => rcv(a.key,convert(a.value))
          case _ => throw new IllegalStateException(s"a map must receive a ${classOf[Assoc[_,_]]} as data")
        }
        override def rcv(x:Any) = throw new IllegalStateException()
        override def rcv(key:Any,value:Any):Unit = {
          if (kConvert==null) kConvert = getSolver(key.getClass,kType)
          super.rcv(kConvert(key) -> value)
        }
      }
    }
 
    override def build(on:AnyRef):I = new Analyze(0,null).newInstance(on,null)
  }

  /** Factory that builds a Binder with a given DataActor */
  final def apply(what:DataActor,solver:ConversionSolver,fd:AutoConvertData,isCol:Boolean):Binder = {
    if (isCol) new CollectionBinder(what,solver,fd)
    else       new Binder(what,solver,fd)
  }
  
  /** A class that lets use Binders easily to enter values directly.
   *  See the test code for example of use.
   *  - method o is for specifying final inputs
   *  - method u is for specifying layers
   */
  implicit final class Helper(val x: Binder#Analyze#Instance) {
    @inline private def sub(f: Helper=>Unit*)                  = { val c:Helper=x.subInstance; f.foreach(_(c)); c.x }
    @inline final def u(f: Helper=>Unit*):Unit                 = sub(f:_*).close()
    @inline final def read                                     = x.read()
  }
  object Helper {
    @inline final def o(v:Any*):Helper=>Unit                   = xh=>v.foreach(xh.x.set(_))
    @inline final def u(f:Helper=>Unit*):Helper=>Unit          = _.u(f:_*)
    @inline final def u(key:Any)(f:Helper=>Unit*):Helper=>Unit = _.sub(f:_*).close(key)
  }
  
}


