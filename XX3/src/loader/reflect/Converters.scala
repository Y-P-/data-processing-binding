package loader.reflect

import scala.reflect.ClassTag
import utils.StringConverter._
import loader.core.definition.Def
import loader.core.context.FieldAnnot
import utils.Reflect._
import java.lang.reflect.Constructor
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import loader.annotations.TagEnd

object Converters {
  
  /**
   * Defines the generic conversion required when building object:
   * - convert String data to appropriate type, depending on the current filed being fed (fd) and the receiving element (e)
   *   this is the most common occurence and merits special treatment
   * - convert any other data (sub-object) to appropriate type, depending on the current filed being fed (fd) and the receiving element (e)
   *   this would happen when a field is itself an object that was not created from a simple string
   *   generic types are not supported (because all generics of a raw class project on that class) 
   */
  trait Converter[-U,V,-E<:Def#Elt] {
    def coerce(dst:Class[_<:V],fd:FieldAnnot):(U,E)=>V
    def apply(dst:Class[_],fd:FieldAnnot):(U,E)=>V = coerce(dst.asInstanceOf[Class[V]],fd)
  }
  
  abstract class StringConverter[V:ClassTag] extends Converter[String,V,Def#Elt]
  
  protected[this] implicit def toConverter[S](f:String=>S) = (s:String,e:Def#Elt) => f(s)
  protected[this] implicit def toMapElt[U:ClassTag](sc:StringConverter[U]) = (implicitly[ClassTag[U]].runtimeClass,sc)
  
  //bridge from FieldAnnot to standard converters. 
  object CvvString extends StringConverter[String] {
    def coerce(dst:Class[_<:String],fd:FieldAnnot) = CvString(fd.check)
  }
  object CvvCharArray extends StringConverter[Array[Char]] {
    def coerce(dst:Class[_<:Array[Char]],fd:FieldAnnot) = CvCharArray(fd.check)
  }
  object CvvInt extends StringConverter[Int] {
    def coerce(dst:Class[_<:Int],fd:FieldAnnot) = CvInt(fd.valid,fd.check,fd.param)
  }
  object CvvJInt extends StringConverter[java.lang.Integer] {
    def coerce(dst:Class[_<:java.lang.Integer],fd:FieldAnnot) = CvJInt(fd.valid,fd.check,fd.param)
  }
  object CvvLong extends StringConverter[Long] {
    def coerce(dst:Class[_<:Long],fd:FieldAnnot) = CvLong(fd.valid,fd.check,fd.param)
  }
  object CvvJLong extends StringConverter[java.lang.Long] {
    def coerce(dst:Class[_<:java.lang.Long],fd:FieldAnnot) = CvJLong(fd.valid,fd.check,fd.param)
  }
  object CvvShort extends StringConverter[Short] {
    def coerce(dst:Class[_<:Short],fd:FieldAnnot) = CvShort(fd.valid,fd.check,fd.param)
  }
  object CvvJShort extends StringConverter[java.lang.Short] {
    def coerce(dst:Class[_<:java.lang.Short],fd:FieldAnnot) = CvJShort(fd.valid,fd.check,fd.param)
  }
  object CvvByte extends StringConverter[Byte] {
    def coerce(dst:Class[_<:Byte],fd:FieldAnnot) = CvByte(fd.valid,fd.check,fd.param)
  }
  object CvvJByte extends StringConverter[java.lang.Byte] {
    def coerce(dst:Class[_<:java.lang.Byte],fd:FieldAnnot) = CvJByte(fd.valid,fd.check,fd.param)
  }
  object CvvChar extends StringConverter[Char] {
    def coerce(dst:Class[_<:Char],fd:FieldAnnot) = CvChar(fd.valid,fd.check)
  }
  object CvvJChar extends StringConverter[java.lang.Character] {
    def coerce(dst:Class[_<:java.lang.Character],fd:FieldAnnot) = CvJChar(fd.valid,fd.check)
  }
  object CvvFloat extends StringConverter[Float] {
    def coerce(dst:Class[_<:Float],fd:FieldAnnot) = CvFloat(fd.valid,fd.check)
  }
  object CvvJFloat extends StringConverter[java.lang.Float] {
    def coerce(dst:Class[_<:java.lang.Float],fd:FieldAnnot) = CvJFloat(fd.valid,fd.check)
  }
  object CvvDouble extends StringConverter[Double] {
    def coerce(dst:Class[_<:Double],fd:FieldAnnot) = CvDouble(fd.valid,fd.check)
  }
  object CvvJDouble extends StringConverter[java.lang.Double] {
    def coerce(dst:Class[_<:java.lang.Double],fd:FieldAnnot) = CvJDouble(fd.valid,fd.check)
  }
  object CvvBoolean extends StringConverter[Boolean] {
    def coerce(dst:Class[_<:Boolean],fd:FieldAnnot) = { val s=fd.param.split("@"); CvBoolean(s(0),s(1)) }
  }
  object CvvJBoolean extends StringConverter[java.lang.Boolean] {
    def coerce(dst:Class[_<:java.lang.Boolean],fd:FieldAnnot) = {
      val x = if (fd.param==null || fd.param.length==0) "yes|oui|vrai|true|1|y|o|v|t@no|non|faux|false|0|n|f" else fd.param
      val s=x.split("@")
      CvJBoolean(s(0),s(1))
    }
  }
  object CvvURL extends StringConverter[java.net.URL] {
    def coerce(dst:Class[_<:java.net.URL],fd:FieldAnnot) = CvURL(fd.check,fd.valid=="C")
  }
  object CvvURI extends StringConverter[java.net.URI] {
    def coerce(dst:Class[_<:java.net.URI],fd:FieldAnnot) = CvURI(fd.check)
  }
  object CvvDate extends StringConverter[java.util.Date] {
    def coerce(dst:Class[_<:java.util.Date],fd:FieldAnnot) = CvDate(fd.check,fd.param)
  }
  object CvvFile extends StringConverter[java.io.File] {
    def coerce(dst:Class[_<:java.io.File],fd:FieldAnnot) = CvFile(fd.valid,fd.check)
  }
  object CvvClass extends StringConverter[Class[_]] {
    def coerce(dst:Class[_<:Class[_]],fd:FieldAnnot) = CvClass(fd.valid,fd.check)
  }
  object CvvEnum extends StringConverter[Enumeration#Value] {
    def coerce(dst:Class[_<:Enumeration#Value],fd:FieldAnnot) = new CvEnumeration[Enumeration#Value]()(ClassTag(dst))(fd.check)
  }
  object CvvJEnum extends StringConverter[Enum[_]] {
    //Note that Enum[_] is generic, but abstract. Concrete classes (i.e. java enums) are not generic, and these will be effectively
    //tested. So in a way the "no generics" contract is not broken here.
    def coerce(dst:Class[_<:Enum[_]],fd:FieldAnnot) = new CvEnum[Enum[_]]()(ClassTag(dst))(fd.check)
  }
 
  //the default ClassMap for string converters.
  val defaultMap = utils.ClassMap(CvvString,CvvCharArray,CvvInt,CvvJInt,CvvShort,CvvJShort,CvvLong,CvvJLong,
                                  CvvByte,CvvJByte,CvvChar,CvvJChar,CvvFloat,CvvJFloat,CvvDouble,CvvJDouble,
                                  CvvURL,CvvURI,CvvDate,CvvFile,CvvEnum,CvvJEnum,CvvBoolean,CvvJBoolean)
  
                                                                  
  //m is a method with no param, or one to three params in (Class[V],FieldAnnot,Def#Elt) (in this order)
  final protected class MethodConverter1[U<:AnyRef,V,-E<:Def#Elt](m:Method) extends Converter[U,V,E] {
    protected[this] val permut = checkParams(p3,m.getParameterTypes,0)
    def coerce(dst:Class[_<:V],fd:FieldAnnot):(U,E)=>V = (u,e)=>m.invoke(u,buildParams(Array(dst,fd,e),permut)).asInstanceOf[V]
  }
  //m is a method with one to four params in (U, Class[V],FieldAnnot,Def#Elt) (in this order, U being mandatory)
  //m can be either static, or belong to some class, of which one instance will be spawned in order to serve for the invocation
  final protected class MethodConverter2[U<:AnyRef,V,-E<:Def#Elt](src:RichClass[U],m:Method) extends Converter[U,V,E] {
    def coerce(dst:Class[_<:V],fd:FieldAnnot):(U,E)=>V = {
      val params = p4(src)
      val helper = if (Modifier.isStatic(m.getModifiers)) {
        //static: find a matching method on the 4 params (src,Class,FieldAnnot,Def#Elt), src being first and mandatory
        (null,null)
      } else {
        val in = ^(m.getDeclaringClass)
        if (in.asObject != null) (in.asObject,null)                     //return a singleton object
        else in.findConstructor(Array(czClass,czFieldAnnot),0) match {  //instance: find a matching constructor on possibly 2 params (Class,FieldAnnot)
          case x if x.length>1  => throw new IllegalStateException(s"too many constructor match the accepted entries for ${m.getDeclaringClass}") 
          case x if x.length==0 => throw new IllegalStateException(s"no constructor match the accepted entries for ${m.getDeclaringClass}") 
          case x                => (x(0)._1.newInstance(buildParams(Array(dst,fd),x(0)._2)),x(0)._2) //create an appropriate helper class with the right params
        }
      }
      val permut = checkParams(params,m.getParameterTypes,1)
      if (permut==null) throw new IllegalStateException(s"method $m cannot be used to convert from $src to $dst")
      if (helper._2!=null) for (x <- helper._2) if (permut.contains(x)) throw new IllegalStateException(s"method $m cannot be used to convert from $src to $dst (${params(x)} is used both in constructor and method)")
      (u,e)=>m.invoke(helper._1,buildParams(Array(u,dst,fd,e),permut)).asInstanceOf[V]        
    }
  }
  private[this] val czFieldAnnot = ^(classOf[FieldAnnot])
  private[this] val czDefElt     = ^(classOf[Def#Elt])
  private[this] val czClass      = ^(classOf[Class[_]])
  private[this] val p3:Array[RichClass[_]]                 = Array(czClass,czFieldAnnot,czDefElt)
  private[this] def p4(c:RichClass[_]):Array[RichClass[_]] = Array(c,czClass,czFieldAnnot,czDefElt)
  
  //finds a method in class in that is an appropriate converter from U to V. If none satisfy, null is returned.
  //bridge or synthetic methods are rejected (i.e. we accept only user defined methods)
  //a TagEnd eligible method takes precedence over any other method
  def apply[U<:AnyRef,V,E<:Def#Elt](in:RichClass[_], src:RichClass[U], dst:RichClass[V], name:String):Option[Converter[U,V,E]] = {
    //Returns true for a method that satisfies the constraints for being used for conversion to V
    def check(m:Method):Boolean = {
      if (name!=null && m.getName!=name) return false
      if (!(dst>m.getReturnType)) return false
      if (Modifier.isStatic(m.getModifiers) || !(src<m.getDeclaringClass))
        checkParams(p4(src),m.getParameterTypes,1)!=null
      else
        checkParams(p3,m.getParameterTypes,1)!=null
    }
    val l = in.filterMethod(check)
    var hasTagEnd = false
    var r:Method = null
    var min:RichClass[_] = null
    for (m <- l) {
      val te = m.getAnnotation(classOf[TagEnd])!=null
      if (te && !hasTagEnd) { hasTagEnd=true; r=m; min=m.getReturnType }  //first tagEnd seen: reinit
      else if ((min==null || min>m.getReturnType) && (te || !hasTagEnd)) { r=m; min=m.getReturnType }
    }
    if (r==null) None else if (src<in.c) Some(new MethodConverter1(r)) else Some(new MethodConverter2(src,r))
  }
}