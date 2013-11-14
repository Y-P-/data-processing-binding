package utils

import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.lang.reflect.Constructor
import java.lang.reflect.AccessibleObject
import scala.reflect.ClassTag
import java.lang.reflect.Field

object Reflect {
  class NoSuchMethodException(msg:String=null) extends Exception(msg)
  class TooManyMethodException(msg:String=null) extends Exception(msg)
  
  /** Provides some Java reflection utilities.
   *  Note that this class doesn't achieve anything close to scala reflection.
   *  It doesn't use any compile time info and is not suitable for general use with generics.
   */
  implicit final class RichClass[U](val c:Class[U]) {
    val isFinal = Modifier.isFinal(c.getModifiers())
    //scala singleton associated with this class if appropriate
    lazy val asObject:U = (try {
      val f = c.getDeclaredField("MODULE$")
      val m = f.getModifiers
      if (Modifier.isFinal(m) && Modifier.isStatic(m)) f.get(null) else null
    } catch {
      case _:Throwable => null
    }).asInstanceOf[U]
    //subclass checks
    // !!! Not for use with generic types.
    //     These are JVM checks, not scala Types checks. Two 'unrelated' classes can thus be
    //     found in relation to each other when they are not! e.g. List[Double] and List[Method]
    //     both erase to List and are superficially seen as compatible, even though they obviously
    //     do not share much in common.
    def <(c:Class[_]):Boolean = c.isAssignableFrom(this.c)
    def >(c:Class[_]):Boolean = this.c.isAssignableFrom(c)
    final def <(c:RichClass[_]):Boolean = this < c.c
    final def >(c:RichClass[_]):Boolean = this > c.c
    //filters out methods/fields/constructors that satisfy some test
    def filter[X<:AccessibleObject:ClassTag](check:X=>Boolean):Array[X] = onAccessible[X,Array[X]](_.filter(check))
    //filters out methods/fields/constructors that satisfy some test
    def find[X<:AccessibleObject:ClassTag](check:X=>Boolean):Option[X] = onAccessible[X,Option[X]](_.find(check))
    //finds appropriate constructors matching the expected class list, whatever order, expect for the mandatory first classes that must be in the correct order. Generics are out.
    def findConstructor(expected:Array[RichClass[_]],mandatory:Int):Array[(Constructor[U],Array[Int])] =
      Reflect.findConstructor(c.getConstructors.asInstanceOf[Array[Constructor[U]]],expected,mandatory)
    //finds the constructor matching the expected class list. Generics are out.
    def findConstructorN(expected:RichClass[_]*):Option[Constructor[U]] =
      Reflect.findConstructor(c.getConstructors.asInstanceOf[Array[Constructor[U]]],expected.toArray,expected.length) match {
        case Array()      => None
        case Array((c,_)) => Some(c)
    }
    override def toString       = s"RichClass[${c.getCanonicalName}]"
    override def equals(o:Any)  = if (o.isInstanceOf[RichClass[_]]) o.asInstanceOf[RichClass[_]].c eq this.c else false
    override def hashCode       = c.hashCode
    
    def onAccessible[X<:AccessibleObject:ClassTag,U](f: AccessibleElements[X]=>U):U = {
      val x = implicitly[ClassTag[X]].runtimeClass
      val o = if      (x==classOf[Method])          methods
              else if (x==classOf[Field])           fields
              else if (x==classOf[Constructor[_]])  constructors
              else                                  null
      f(o.asInstanceOf[AccessibleElements[X]])
    }

    abstract class AccessibleElements[X<:AccessibleObject:ClassTag] {
      def get:Array[X]
      def getDeclared:Array[X]
      def getModifiers(x:X):Int
      def isSynthetic(x:X):Boolean
      def isBridge(x:X):Boolean
      def getName(x:X):String
      private[this] def keep(x:X) = !(isSynthetic(x) || isBridge(x))
      def debug(x:X) = {
        val m = getModifiers(x)
        print(getName(x))
        if (Modifier.isPublic(m)) print(" public")
        if (Modifier.isPrivate(m)) print(" private")
        if (Modifier.isProtected(m)) print(" protected")
        if (isSynthetic(x)) print(" synthetic")
        if (isBridge(x)) print(" bridge") 
      }
      def filter(check:X=>Boolean):Array[X] = {
        val r = new Array[AnyRef](getDeclared.length+get.length)
        var i=0
        //protected/private local methods
        for (m <- get) if (!Modifier.isPublic(getModifiers(m)) && keep(m) && check(m)) { r(i)=m; i+=1 }
        //public methods (incl. inherited)
        for (m <- getDeclared) if (keep(m) && check(m)) { r(i)=m; i+=1 }
        val r0 = new Array[X](i)
        System.arraycopy(r, 0, r0, 0, i)
        r0
      }
      //find out the first method/field/constructor that satisfy some test
      def find(check:X=>Boolean):Option[X] = {
        def f(a:Array[X]):Option[X] = { for (x <- a) if (keep(x) && check(x)) return Some(x); None }
        f(get).orElse(f(getDeclared))
      }
    }
    object methods extends AccessibleElements[Method] {
      def get                    = c.getMethods
      def getDeclared            = c.getDeclaredMethods
      def getModifiers(x:Method) = x.getModifiers
      def isSynthetic(x:Method)  = x.isSynthetic
      def isBridge(x:Method)     = x.isBridge
      def getName(x:Method)      = x.getName
    }
    object fields extends AccessibleElements[Field] {
      def get                    = c.getFields
      def getDeclared            = c.getDeclaredFields
      def getModifiers(x:Field)  = x.getModifiers
      def isSynthetic(x:Field)   = x.isSynthetic
      def isBridge(x:Field)      = false
      def getName(x:Field)       = x.getName
    }
    object constructors extends AccessibleElements[Constructor[_]] {
      def get                            = c.getConstructors
      def getDeclared                    = c.getDeclaredConstructors
      def getModifiers(x:Constructor[_]) = x.getModifiers
      def isSynthetic(x:Constructor[_])  = x.isSynthetic
      def isBridge(x:Constructor[_])     = false
      def getName(x:Constructor[_])      = x.getName
    }
  }
  
  //easy factory
  def ^[U](c:Class[U]) = new RichClass[U](c)
  
  /** Finds the method in an array that is closest to the parameter/return types given.
   *  The parameter checked is the first.
   *  The returned method has the minimal parameter type then maximum return type admissible
   *  Note that this is not intended to deal with complex types (generics most notably.)
   *  @param a, an array of method to check
   *  @param src, the first parameter expected class. Can be null if that is not to be checked.
   *  @param dst, the return class. Can be null if that is not to be checked.
   */
  def reduce(a:Array[Method],src:RichClass[_],dst:RichClass[_]):Array[Method] = {
    var s = src
    val l1 = if (src==null) a else {
      //find minimal class for source
      for (m <- a if dst==null || dst>m.getReturnType()) { val x=m.getParameterTypes()(0); if (s>x) s=x }
      //build sublist with minimal class for source
      for (m <- a if s>m.getParameterTypes()(0)) yield m
    }
    val l = if (l1.length<=1 && dst!=null) l1 else {
      s = null
      //find maximal class for return type
      for (m <- l1) { val x=m.getReturnType(); if (s==null || s<x) s=x }
      //build sublist with maximal class for return type
      for (m <- a if s<m.getReturnType()) yield m
    }
    l
  }
  
  /** Finds a method (static or not) in src that return dst (or a subclass of).
   *  @param  src, the source class in which we are looking for an appropriate method (possibly static)
   *  @param  dst, the class to return
   *  @param  check, a method that adds additional criterion on a method (such as name...)
   *  @return the list of matching Converter
   *  @throws NoSuchMethodException if no method or more than one method is found matching
   */
  def find[U<:AnyRef,V](src:RichClass[U],dst:RichClass[V],check:(Method)=>Boolean):Array[Method] =
    reduce(src.filter(m => check(m) && dst>m.getReturnType),src,dst)
    
  ////////////////////////////////////////////////////////////////////////////////////////////////  
  // The following methods are useful to deal with reflexion around variable list of parameters //
  ////////////////////////////////////////////////////////////////////////////////////////////////  
    
  /** Matches an incoming list of classes against an expected list of classes. Classes must be unrelated or the match will be unpredictable.
   *  @param expected, the list of possible classes
   *  @param incoming, the list of found classes
   *  @param mandatory, the number of elements in expected that must be found in the right order in found
   *  @return an array indicating how indexes in incoming match indexes in expected. null if fails (i.e some elts in incoming don't match expected)
   */
  def checkParams(expected:Array[RichClass[_]],incoming:Array[Class[_]],mandatory:Int):Array[Int] = {
    if (incoming.length>expected.length) return null
    val a = new Array[Int](incoming.length)
    for (i <- 0 until mandatory) if (!(expected(i)>incoming(i))) return null else a(i)=i
    for (i <- mandatory until incoming.length) {
      var k = -1
      for (j <- mandatory until expected.length if k<0 && expected(j)>incoming(i)) k=j
      if (k<0) return null
      a(i)=k
    }
    a
  }
  /** Builds the actual parameter array from a list of possible parameters, based on the substitution array 'matching' (likely coming
   *  from a call to the previous method)
   */
  def buildParams(possible:Array[AnyRef],matching:Array[Int]):Array[AnyRef] = {
    val a = new Array[AnyRef](matching.length)
    for (i <- 0 until a.length) a(i) = possible(matching(i))
    a
  }
  
  /** restricts 'in' to the Methods that do accept the right kind of parameters */
  def findMethod(in:Array[Method],expected:Array[RichClass[_]],mandatory:Int):Array[(Method,Array[Int])] =
    for (m <- in; p=checkParams(expected,m.getParameterTypes,mandatory) if p!=null) yield (m,p)
  /** restricts 'in' to the Constructors that do accept the right kind of parameters */
  def findConstructor[U](in:Array[Constructor[U]],expected:Array[RichClass[_]],mandatory:Int):Array[(Constructor[U],Array[Int])] =
    for (m <- in; p=checkParams(expected,m.getParameterTypes,mandatory) if p!=null) yield (m,p)
  
  /** returns true if p1 and p2 represent the same primitive type, Java Boxed or not */
  final def checkPrimitive(p1:Class[_],p2:Class[_]):Boolean = {
    if (p1 eq p2)                       return true
    if (!p1.isPrimitive)                return p2.isPrimitive && checkPrimitive(p2,p1)
    if (p1 eq java.lang.Integer.TYPE)   return p2 eq classOf[java.lang.Integer]
    if (p1 eq java.lang.Boolean.TYPE)   return p2 eq classOf[java.lang.Boolean]
    if (p1 eq java.lang.Character.TYPE) return p2 eq classOf[java.lang.Character]
    if (p1 eq java.lang.Float.TYPE)     return p2 eq classOf[java.lang.Float]
    if (p1 eq java.lang.Double.TYPE)    return p2 eq classOf[java.lang.Double]
    if (p1 eq java.lang.Short.TYPE)     return p2 eq classOf[java.lang.Short]
    if (p1 eq java.lang.Byte.TYPE)      return p2 eq classOf[java.lang.Byte]
    if (p1 eq java.lang.Long.TYPE)      return p2 eq classOf[java.lang.Long]
    false
  }
      
      //XXX for fun...
      def copy[T<:AnyRef:scala.reflect.ClassTag](b:T,p1: String):T = {
        import scala.reflect.runtime.{ currentMirror => cm }
        import scala.reflect.runtime.universe._
        val im = cm.reflect(b)
        val ts = im.symbol.typeSignature
        val copySym = ts.member(newTermName("copy")).asMethod
        def element(p: Symbol): Any = (im reflectMethod ts.member(p.name).asMethod)()
        val args = for (ps <- copySym.paramss; p <- ps) yield {
          if (p.name.toString == "p1") p1 else element(p)
        }
        (im reflectMethod copySym)(args: _*).asInstanceOf[T]
      }      
     
}