package utils

import java.lang.reflect.Method
import java.lang.reflect.Modifier
import java.lang.reflect.Constructor

object Reflect {
  class NoSuchMethodException(msg:String=null) extends Exception(msg)
  class TooManyMethodException(msg:String=null) extends Exception(msg)
  
  /** Provides some Java reflection utilities.
   *  Note that this class doesn't achieve anything close to scala reflection.
   *  It doesn't use any compile time info.
   */
  implicit final class RichClass[U](val c:Class[U]) {
    val isFinal = Modifier.isFinal(c.getModifiers())
    
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
    def <(c:RichClass[_]):Boolean = this < c.c
    def >(c:RichClass[_]):Boolean = this > c.c
    //filters out methods that satisfy some test
    def filterMethod(check:(Method)=>Boolean):Array[Method] = {
      //protected/private local methods
      val r1 = for (m <- c.getDeclaredMethods if !Modifier.isPublic(m.getModifiers) && !m.isSynthetic && !m.isBridge && check(m))
        yield m
      //public methods (incl. inherited)
      val r2 = for (m <- c.getMethods if !m.isSynthetic && !m.isBridge && check(m))
        yield m
      r1 ++ r2
    }
    def findConstructor(expected:Array[RichClass[_]],mandatory:Int):Array[(Constructor[U],Array[Int])] =
      Reflect.findConstructor(c.getConstructors.asInstanceOf[Array[Constructor[U]]],expected,mandatory)
      
    override def equals(o:Any)  = if (o.isInstanceOf[RichClass[_]]) o.asInstanceOf[RichClass[_]].c eq this.c else false
    override def hashCode       = c.hashCode
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
    reduce(src.filterMethod(m => check(m) && dst>m.getReturnType),src,dst)
    
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
    for (i <- 0 to mandatory) if (!(expected(i)>incoming(i))) return null else a(i)=i
    for (i <- mandatory to incoming.length) {
      var k = -1
      for (j <- mandatory to expected.length if k<0 && expected(j)>incoming(i)) k=j
      if (k<0) return null
      a(i)=k
    }
    a
  }
  /** Builds the actual parameter array from a list of possible parameters, based on the substituion array 'matching' (likely coming
   *  from a call to the previous method)
   */
  def buildParams(possible:Array[AnyRef],matching:Array[Int]):Array[AnyRef] = {
    val a = new Array[AnyRef](matching.length)
    for (i <- 0 to a.length) a(i) = possible(matching(i))
    a
  }
  
  /** restricts 'in' to the Methods that do accept the right kind of parameters */
  def findMethod(in:Array[Method],expected:Array[RichClass[_]],mandatory:Int):Array[(Method,Array[Int])] =
    for (m <- in; p=checkParams(expected,m.getParameterTypes,mandatory) if p!=null) yield (m,p)
  /** restricts 'in' to the Constructors that do accept the right kind of parameters */
  def findConstructor[U](in:Array[Constructor[U]],expected:Array[RichClass[_]],mandatory:Int):Array[(Constructor[U],Array[Int])] =
    for (m <- in; p=checkParams(expected,m.getParameterTypes,mandatory) if p!=null) yield (m,p)
    
      
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