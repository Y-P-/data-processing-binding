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
  object cannotCompare extends Exception { override def fillInStackTrace = this }

  abstract class AccessibleElements[X<:AccessibleObject:ClassTag](l:Array[X]) {
    implicit val wrapped:scala.collection.mutable.ArrayOps[X] = l
    def getModifiers(x:X):Int
    def isSynthetic(x:X):Boolean
    def isBridge(x:X):Boolean
    def getName(x:X):String
    def <(x1:X,x2:X):Boolean
    def debug(x:X) = {
      val m = getModifiers(x)
      print(getName(x))
      if (Modifier.isPublic(m)) print(" public")
      if (Modifier.isPrivate(m)) print(" private")
      if (Modifier.isProtected(m)) print(" protected")
      if (isSynthetic(x)) print(" synthetic")
      if (isBridge(x)) print(" bridge") 
    }
    final def min:X = {
      var min:X = null.asInstanceOf[X]
      for (x <- wrapped) {
        if (min==null || <(x,min)) min=x                  //first item, or smaller item
        else if (! <(min,x)) return null.asInstanceOf[X]  //item is not comparable: failure
      }
      min
    }
  }
  object AccessibleElements {
    //Factory for the limited number of AccessibleObject classes
    def apply[X<:AccessibleObject:ClassTag](l:Array[X]):AccessibleElements[X] = {
      val x = implicitly[ClassTag[X]].runtimeClass
      (if      (x==classOf[Method])          new Methods(l.asInstanceOf[Array[Method]])
       else if (x==classOf[Field])           new Fields(l.asInstanceOf[Array[Field]])
       else if (x==classOf[Constructor[_]])  new Constructors(l.asInstanceOf[Array[Constructor[_]]])
       else                                  null).asInstanceOf[AccessibleElements[X]]
    }
    implicit def toArray[X<:AccessibleObject](a:AccessibleElements[X]) = a.wrapped
  }
  
  def <(m1:Method,m2:Method):Boolean = {
    val p1 = m1.getParameterTypes
    val p2 = m1.getParameterTypes
    if (p1.length!=p2.length) return false
    for (i <- 0 until p1.length) if (!p2(i).isAssignableFrom(p1(i))) return false
    return true
  }
  def <(m1:Constructor[_],m2:Constructor[_]):Boolean = {
    val p1 = m1.getParameterTypes
    val p2 = m1.getParameterTypes
    if (p1.length!=p2.length) return false
    for (i <- 0 until p1.length) if (!p2(i).isAssignableFrom(p1(i))) return false
    return true
  }
  class Methods(l:Array[Method]) extends AccessibleElements[Method](l) {
    final def getModifiers(x:Method)         = x.getModifiers
    final def isSynthetic(x:Method)          = x.isSynthetic
    final def isBridge(x:Method)             = x.isBridge
    final def getName(x:Method)              = x.getName
    final def <(m1:Method,m2:Method):Boolean = Reflect.<(m1,m2)
  }
  class Fields(l:Array[Field]) extends AccessibleElements[Field](l) {
    final def getModifiers(x:Field)        = x.getModifiers
    final def isSynthetic(x:Field)         = x.isSynthetic
    final def isBridge(x:Field)            = false
    final def getName(x:Field)             = x.getName
    final def <(m1:Field,m2:Field):Boolean = false
  }
  class Constructors(l:Array[Constructor[_]]) extends AccessibleElements[Constructor[_]](l) {
    final def getModifiers(x:Constructor[_]) = x.getModifiers
    final def isSynthetic(x:Constructor[_])  = x.isSynthetic
    final def isBridge(x:Constructor[_])     = false
    final def getName(x:Constructor[_])      = x.getName
    final def <(c1:Constructor[_],c2:Constructor[_]):Boolean = Reflect.<(c1,c2)
  }
    
  /** Provides some Java reflection utilities.
   *  Note that this class doesn't achieve anything close to scala reflection.
   *  It doesn't use any compile time info and is not suitable for general use with generics.
   */
  implicit final class RichClass[+U](val c:Class[_<:U]) {
    final val isFinal = Modifier.isFinal(c.getModifiers())
    //scala singleton associated with this class if appropriate
    lazy val asObject:U = (try {
      val x = if (c.getEnclosingClass!=null) Class.forName(c.getEnclosingClass.getName+"$"+c.getSimpleName)
              else c
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
    //finds appropriate constructors matching the expected class list, whatever order, expect for the mandatory first classes that must be in the correct order. Generics are out.
    def findConstructor(expected:Array[RichClass[_]],mandatory:Int):Array[_<:(_<:Constructor[_<:U],Array[Int])] =
      Reflect.findConstructor[U](c.getConstructors.asInstanceOf[Array[_<:Constructor[_<:U]]],expected,mandatory)
    //finds the constructor matching the expected class list. Generics are out.
    def findConstructorN(expected:RichClass[_]*):Option[Constructor[_<:U]] =
      Reflect.findConstructor[U](c.getConstructors.asInstanceOf[Array[_<:Constructor[_<:U]]],expected.toArray,expected.length) match {
        case Array()      => None
        case Array((c,_)) => Some(c)
    }
    
    final def printMethods() = methods.foreach(println)
    override def toString       = s"RichClass[${c.getCanonicalName}]"
    override def equals(o:Any)  = if (o.isInstanceOf[RichClass[_]]) o.asInstanceOf[RichClass[_]].c eq this.c else false
    override def hashCode       = c.hashCode
    
    //standard way to retrieve useful methods,fields and construtors
    //this will retrieve all public methods from the class and superclasses, and all methods from the class itself (protected or private)
    //a method is present only once
    //synthetic methods are excluded
    def methods      = (c.getDeclaredMethods.filter(m => !Modifier.isPublic(m.getModifiers))++c.getMethods).filter(!_.isSynthetic)
    def fields       = (c.getDeclaredFields.filter(m => !Modifier.isPublic(m.getModifiers))++c.getFields).filter(!_.isSynthetic)
    def constructors = (c.getDeclaredConstructors.filter(m => !Modifier.isPublic(m.getModifiers))++c.getConstructors).filter(!_.isSynthetic)
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
  
  /** Finds the methods (static or not) in src that return dst (or a subclass of).
   *  @param  src, the source class in which we are looking for an appropriate method (possibly static)
   *  @param  dst, the class to return
   *  @param  check, a method that adds additional criterion on a method (such as name...)
   *  @return the list of matching Converter
   *  @throws NoSuchMethodException if no method or more than one method is found matching
   */
  def find[U<:AnyRef,V](src:RichClass[U],dst:RichClass[V],check:(Method)=>Boolean):Array[Method] =
    reduce(src.methods.filter(m => check(m) && dst>m.getReturnType),src,dst)
    
  ////////////////////////////////////////////////////////////////////////////////////////////////  
  // The following methods are useful to deal with reflexion around variable list of parameters //
  ////////////////////////////////////////////////////////////////////////////////////////////////  
    
  /** Matches an incoming list of classes against an expected list of classes. Classes must be unrelated or the match will be unpredictable.
   *  @param expected, the list of possible classes
   *  @param incoming, the list of found classes ; they must all be assignable to at most one of the expected classes.
   *                   it is possible to only partially match the incoming list, but the expected list must be fully met.
   *  @param mandatory, the number of elements in expected that must be found in the right order in found
   *  @return an array indicating how indexes in incoming match indexes in expected. null if fails (i.e some incoming elts don't match any expected one)
   */
  def checkParams(expected:Array[Class[_]],incoming:Array[RichClass[_]],mandatory:Int):Array[Int] = {
    if (incoming.length<expected.length) return null
    val a = new Array[Int](expected.length)
    //mandatory arguments must be matched at their exact position
    for (i <- 0 until mandatory) if (!(incoming(i)<expected(i))) return null else a(i)=i
    val r = mandatory until incoming.length
    //loop on other expected arguments
    for (i <- mandatory until expected.length) {               
      if (r.find(incoming(_)<expected(i)).map(a(i)=_)==None)   //check if an incoming argument matches and if found, record its index
        return null                                            //if none found, arguments match fails
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
  def findMethod(in:Array[Method],incoming:Array[RichClass[_]],mandatory:Int):Array[(Method,Array[Int])] =
    for (m <- in; p=checkParams(m.getParameterTypes,incoming,mandatory) if p!=null) yield (m,p)
  /** restricts 'in' to the Constructors that do accept the right kind of parameters */
  def findConstructor[U](in:Array[_<:Constructor[_<:U]],incoming:Array[RichClass[_]],mandatory:Int):Array[(Constructor[_<:U],Array[Int])] =
    for (m <- in; p=checkParams(m.getParameterTypes,incoming,mandatory) if p!=null) yield (m,p)
    
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