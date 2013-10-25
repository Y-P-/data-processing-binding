package utils

import java.lang.reflect.Method
import java.lang.reflect.Modifier

object Reflect {
  class NoSuchMethodException(msg:String=null) extends Exception(msg)
  class TooManyMethodException(msg:String=null) extends Exception(msg)
  
  implicit class RichClass[U](val c:Class[U]) {
    def <(c:Class[_]):Boolean = c.isAssignableFrom(this.c)
    def <[X:Manifest]:Boolean = { manifest[X].erasure.isAssignableFrom(this.c) }
    def >(c:Class[_]):Boolean = this.c.isAssignableFrom(c)
    def >[X:Manifest]:Boolean = { this.c.isAssignableFrom(manifest[X].erasure) }
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
  }
  /** Finds the method in an array that is closest to the parameter/return types given.
   *  The parameter checked is the first.
   *  The returned method has the minimal parameter type then maximum return type admissible
   *  Note that this is not intended to deal with complex types (generics most notably.)
   *  @param a, an array of method to check
   *  @param src, the first parameter expected class. Can be null if that is not to be checked.
   *  @param dst, the return class. Can be null if that is not to be checked.
   */
  def reduce(a:Array[Method],src:RichClass[_],dst:RichClass[_]):Method = {
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
    if (l.length==0) throw new NoSuchMethodException
    if (l.length!=1) throw new TooManyMethodException
    l(0)
  }
  
  /** Finds a method in src (static or not) that return dst (or a subclass of) and build a converter with it.
   *  @param  src, the source class in which we are looking for an appropriate method (possibly static)
   *  @param  dst, the class to convert to
   *  @param  check, a method that adds additional criterion on a method (such as name...)
   *  @return the unique matching Converter
   *  @throws NoSuchMethodException if no method or more than one method is found matching
   */
  def find[U<:AnyRef,V](src:RichClass[U],dst:RichClass[V],check:(Method)=>Boolean):Method =
    reduce(src.filterMethod(m => check(m) && dst>m.getReturnType),src,dst)
}