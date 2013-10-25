package utils

import java.lang.reflect.Method
import java.lang.reflect.Modifier

object AutoConvert {
  import Reflect.RichClass
  
  /** Defines a conversion from U to V using a string parameter as a guide for conversion.
   */
  trait Converter[U<:AnyRef,V] {
    def src:RichClass[U]
    def dst:RichClass[V]
    def apply(u:U,param:String):V
    def defaultName = src.c.getName+"-"+dst.c.getName
  }
  //case 1: m is a U-class method with no param (string ignored)
  final protected class Converter1[U<:AnyRef,V](m:Method) extends Converter[U,V] {
    val src = new RichClass(m.getDeclaringClass().asInstanceOf[Class[U]])
    val dst = new RichClass(m.getReturnType().asInstanceOf[Class[V]])
    def apply(u:U,param:String):V = m.invoke(u).asInstanceOf[V]
  }
  //case 2: m is a U-class method with one string param
  final protected class Converter2[U<:AnyRef,V](m:Method) extends Converter[U,V] {
    val src = new RichClass(m.getDeclaringClass().asInstanceOf[Class[U]])
    val dst = new RichClass(m.getReturnType().asInstanceOf[Class[V]])
    def apply(u:U,param:String):V = m.invoke(u,param).asInstanceOf[V]
  }
  //case 3: m is a static method with one U param (string ignored)
  final protected class Converter3[U<:AnyRef,V](m:Method) extends Converter[U,V] {
    val src = new RichClass(m.getParameterTypes()(0).asInstanceOf[Class[U]])
    val dst = new RichClass(m.getReturnType().asInstanceOf[Class[V]])
    def apply(u:U,param:String):V = m.invoke(null,Array[AnyRef](u)).asInstanceOf[V]
  }
  //case 4: m is a U-class method with one string param
  final protected class Converter4[U<:AnyRef,V](m:Method) extends Converter[U,V] {
    val src = new RichClass(m.getParameterTypes()(0).asInstanceOf[Class[U]])
    val dst = new RichClass(m.getReturnType().asInstanceOf[Class[V]])
    def apply(u:U,param:String):V = m.invoke(null,Array[AnyRef](u,param)).asInstanceOf[V]
  }
  //builds a converter from U to V using m. If m doesn't satisfy, null is returned.
  //bridge or synthetic methods are rejected (i.e. we accept only user defined methods)
  def apply[U<:AnyRef,V](m:Method,src:RichClass[U],dst:RichClass[V]):Converter[U,V] = {
    val l = m.getParameterTypes()
    if (!(dst>m.getReturnType) || m.isBridge || m.isSynthetic) null
    else if (Modifier.isStatic(m.getModifiers)) {
      if (src<l(0)) l.length match {
        case 1                          => new Converter3(m)
        case 2 if l(1)==classOf[String] => new Converter4(m)
        case _                          => null
      } else                               null
    } else {
      if (src<m.getDeclaringClass()) l.length match {
        case 0                          => new Converter1(m)
        case 1 if l(0)==classOf[String] => new Converter2(m)
        case _                          => null
      } else                               null
    }
  }
  def apply[U<:AnyRef,V](m:(U)=>V,src0:RichClass[U],dst0:RichClass[V]):Converter[U,V] = new Converter[U,V] {
    val src = src0
    val dst = dst0
    def apply(u:U,param:String):V = m(u)
  }
  def apply[U<:AnyRef,V](m:(U,String)=>V,src0:RichClass[U],dst0:RichClass[V]):Converter[U,V] = new Converter[U,V] {
    val src = src0
    val dst = dst0
    def apply(u:U,param:String):V = m(u,param)
  }
  
  /** A collection of named Converters. Works more or less like a map.
   */
  class Collection {
    import scala.collection.mutable.ArrayBuffer
    private[this] val k = new ArrayBuffer[String]
    private[this] val a = new ArrayBuffer[Converter[_ <: AnyRef, _]]
    //remove a converter by name
    def -(key: String):Unit = k.indexOf(key) match {
      case -1 =>
      case  i => k.remove(i); a.remove(i)
    }
    //adds a converter
    def put(key:String, value:Converter[_ <: AnyRef, _]):Unit =
      if (key==null)               put(value.defaultName,value)
      else if (-1==k.indexOf(key)) throw new IllegalStateException
      else                         { a+=value; k+=key }
    //finds a converter by name
    def get(key: String): Option[Converter[_ <: AnyRef, _]] = k.indexOf(key) match {
      case -1 => None
      case  i => Some(a(i))
    }
    def getAs[U<:AnyRef,V](key:String,src:Class[U],dst:Class[V]): Option[Converter[U,V]] = k.indexOf(key) match {
      case -1 => None
      case  i => val r=a(i); if (r.src>src && r.dst<dst) Some(a(i).asInstanceOf[Converter[U,V]]) else None
    }
    //finds all converters compatible with src as source
    def getSrc[U<:AnyRef](src: Class[U]):ArrayBuffer[Converter[U, _]]   = (for (i <- a if i.src > src) yield i).asInstanceOf[ArrayBuffer[Converter[U,_]]]
    //finds all converters compatible with dst as result
    def getDst[V](dst: Class[V]):ArrayBuffer[Converter[_ <: AnyRef, V]] = (for (i <- a if i.dst < dst) yield i).asInstanceOf[ArrayBuffer[Converter[_ <: AnyRef,V]]]
    //finds all converters compatible with both src and dst
    def get[U<:AnyRef,V](src:Class[U],dst:Class[V]):ArrayBuffer[Converter[U,V]] = (for (i <- a if i.src > src && i.dst < dst) yield i).asInstanceOf[ArrayBuffer[Converter[U,V]]]
  }
  object Collection {
    def apply[U](x:(String,Converter[_<:AnyRef,_])*):Collection = {
      val c = new Collection
      for (v <- x) c.put(v._1,v._2)
      c
    }
  }
  
  /** A collection of Converters, specialized with a source class.
   */
  class SpecCollection[U<:AnyRef] {
    import scala.collection.mutable.ArrayBuffer
    private[this] val k = new ArrayBuffer[String]
    private[this] val a = new ArrayBuffer[Converter[U, _]]
    //remove a converter by name
    def -(key: String):Unit = k.indexOf(key) match {
      case -1 =>
      case  i => k.remove(i); a.remove(i)
    }
    //adds a converter
    def put(key:String, value:Converter[U,_]):Unit =
      if (key==null)               put(value.dst.c.getName,value)
      else if (-1==k.indexOf(key)) throw new IllegalStateException
      else                         { a+=value; k+=key }
    //finds a converter by name
    def get(key: String): Option[Converter[U,_]] = k.indexOf(key) match {
      case -1 => None
      case  i => Some(a(i))
    }
    def getAs[V](key:String,dst:Class[V]): Option[Converter[U,V]] = k.indexOf(key) match {
      case -1 => None
      case  i => val r=a(i); if (r.dst<dst) Some(a(i).asInstanceOf[Converter[U,V]]) else None
    }
    //finds all converters compatible with src (a subclass of U) as source
    def getSrc[X<:U](src: Class[X]):ArrayBuffer[Converter[X,_]]   = (for (i <- a if i.src > src) yield i).asInstanceOf[ArrayBuffer[Converter[X,_]]]
    //finds all converters compatible with dst as result
    def getDst[V](dst: Class[V]):ArrayBuffer[Converter[U,V]] = (for (i <- a if i.dst < dst) yield i).asInstanceOf[ArrayBuffer[Converter[U,V]]]
    //finds all converters compatible with both src and dst
    def get[X<:U,V](src:Class[X],dst:Class[V]):ArrayBuffer[Converter[X,V]] = (for (i <- a if i.src > src && i.dst < dst) yield i).asInstanceOf[ArrayBuffer[Converter[X,V]]]
  }
  object SpecCollection {
    def apply[U<:AnyRef](x:(String,Converter[U,_])*):SpecCollection[U] = {
      val c = new SpecCollection[U]
      for (v <- x) c.put(v._1,v._2)
      c
    }
  }
}
