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
  trait Converter[-U<:AnyRef,+V,-E<:Def#Elt] {
   def src:RichClass[_>:U]  //the maximal class accepted by that converter
   def dst:RichClass[_<:V]  //the minimal class returned by the converter
   def apply(fd:FieldAnnot):(U,E)=>V
  }
  
  abstract class StringConverter[V](val dst:RichClass[_<:V]) extends Converter[String,V,Def#Elt] {
    def this(s:utils.StringConverter[V]) = this(s.dst)
    val src = ^(classOf[String])
  }
  trait StringConverterBuilder[V] {
    def apply(c:Class[V]):StringConverter[V]
  }
  
  protected[this] implicit def toConverter[S](f:String=>S) = (s:String,e:Def#Elt) => f(s)
  protected[this] implicit def toMapElt[U:ClassTag](sc:StringConverter[U]) = (implicitly[ClassTag[U]].runtimeClass,sc)
  
  //bridge from FieldAnnot to standard converters. 
  object CvvString extends StringConverter(CvString) {
    def apply(fd:FieldAnnot) = CvString(fd.check)
  }
  object CvvCharArray extends StringConverter(CvCharArray) {
    def apply(fd:FieldAnnot) = CvCharArray(fd.check)
  }
  object CvvInt extends StringConverter(CvInt) {
    def apply(fd:FieldAnnot) = CvInt(fd.valid,fd.check,fd.param)
  }
  object CvvJInt extends StringConverter(CvJInt) {
    def apply(fd:FieldAnnot) = CvJInt(fd.valid,fd.check,fd.param)
  }
  object CvvLong extends StringConverter(CvLong) {
    def apply(fd:FieldAnnot) = CvLong(fd.valid,fd.check,fd.param)
  }
  object CvvJLong extends StringConverter(CvJLong) {
    def apply(fd:FieldAnnot) = CvJLong(fd.valid,fd.check,fd.param)
  }
  object CvvShort extends StringConverter(CvShort) {
    def apply(fd:FieldAnnot) = CvShort(fd.valid,fd.check,fd.param)
  }
  object CvvJShort extends StringConverter(CvJShort) {
    def apply(fd:FieldAnnot) = CvJShort(fd.valid,fd.check,fd.param)
  }
  object CvvByte extends StringConverter(CvByte) {
    def apply(fd:FieldAnnot) = CvByte(fd.valid,fd.check,fd.param)
  }
  object CvvJByte extends StringConverter(CvJByte) {
    def apply(fd:FieldAnnot) = CvJByte(fd.valid,fd.check,fd.param)
  }
  object CvvChar extends StringConverter(CvChar) {
    def apply(fd:FieldAnnot) = CvChar(fd.valid,fd.check)
  }
  object CvvJChar extends StringConverter(CvJChar) {
    def apply(fd:FieldAnnot) = CvJChar(fd.valid,fd.check)
  }
  object CvvFloat extends StringConverter(CvFloat) {
    def apply(fd:FieldAnnot) = CvFloat(fd.valid,fd.check)
  }
  object CvvJFloat extends StringConverter(CvJFloat) {
    def apply(fd:FieldAnnot) = CvJFloat(fd.valid,fd.check)
  }
  object CvvDouble extends StringConverter(CvDouble) {
    def apply(fd:FieldAnnot) = CvDouble(fd.valid,fd.check)
  }
  object CvvJDouble extends StringConverter(CvJDouble) {
    def apply(fd:FieldAnnot) = CvJDouble(fd.valid,fd.check)
  }
  object CvvBoolean extends StringConverter(CvBoolean) {
    def apply(fd:FieldAnnot) = { val s=fd.param.split("@"); CvBoolean(s(0),s(1)) }
  }
  object CvvJBoolean extends StringConverter(CvJBoolean) {
    def apply(fd:FieldAnnot) = {
      val x = if (fd.param==null || fd.param.length==0) "yes|oui|vrai|true|1|y|o|v|t@no|non|faux|false|0|n|f" else fd.param
      val s=x.split("@")
      CvJBoolean(s(0),s(1))
    }
  }
  object CvvURL extends StringConverter(CvURL) {
    def apply(fd:FieldAnnot) = CvURL(fd.check,fd.valid=="C")
  }
  object CvvURI extends StringConverter(CvURI) {
    def apply(fd:FieldAnnot) = CvURI(fd.check)
  }
  object CvvDate extends StringConverter(CvDate) {
    def apply(fd:FieldAnnot) = CvDate(fd.check,fd.param)
  }
  object CvvFile extends StringConverter(CvFile) {
    def apply(fd:FieldAnnot) = CvFile(fd.valid,fd.check)
  }
  object CvvClass extends StringConverter(CvClass) {
    def apply(fd:FieldAnnot) = CvClass(fd.valid,fd.check)
  }
  
  object CvvEnum extends utils.ClassMap.Factory[Enumeration#Value,StringConverter[_]] {
    val max = ^(classOf[Enumeration#Value])
    def build[X<:Enumeration#Value](cz:Class[X]) = new StringConverter[X](cz) {
      def apply(fd:FieldAnnot) = new CvEnumeration[X]()(ClassTag(cz))(fd.check)
    }
  }
  object CvvJEnum extends utils.ClassMap.Factory[Enum[_],StringConverter[_]] {
    //Note that Enum[_] is generic, but abstract. Concrete classes (i.e. java enums) are not generic, and these will be effectively
    //tested. So in a way the "no generics" contract is not broken here.
    val max = ^(classOf[Enum[_]])
    def build[X<:Enum[_]](cz:Class[X]) = new StringConverter[X](cz) {
      def apply(fd:FieldAnnot) = new CvEnum[X]()(ClassTag(cz))(fd.check)
    }
  }
 
  //the default ClassMap for string converters.
  val defaultMap = utils.ClassMap[StringConverter[_]](
                        CvvString,CvvCharArray,CvvInt,CvvJInt,CvvShort,CvvJShort,CvvLong,CvvJLong,
                        CvvByte,CvvJByte,CvvChar,CvvJChar,CvvFloat,CvvJFloat,CvvDouble,CvvJDouble,
                        CvvURL,CvvURI,CvvDate,CvvFile,CvvBoolean,CvvJBoolean)(
                        CvvEnum,CvvJEnum
                        )
 

  //m is a method with no param, or one to three params in (Class[V],FieldAnnot,Def#Elt) (in this order)
  final protected class MethodConverter1[-U<:AnyRef,+V,-E<:Def#Elt](val dst:RichClass[_<:V], val src:RichClass[_>:U], m:Method) extends Converter[U,V,E] {
    protected[this] val permut = checkParams(p3,m.getParameterTypes,0)
    def apply(fd:FieldAnnot):(U,E)=>V = {
      (u,e)=>m.invoke(u,buildParams(Array(dst.c,fd,e),permut)).asInstanceOf[V]
    }
  }
  //m is a method with one to four params in (U, Class[V],FieldAnnot,Def#Elt) (in this order, U being mandatory)
  //m can be either static, or belong to some class, of which one instance will be spawned in order to serve for the invocation
  final protected class MethodConverter2[U<:AnyRef,+V,-E<:Def#Elt](val dst:RichClass[_<:V], val src:RichClass[_>:U], m:Method) extends Converter[U,V,E] {
    def apply(fd:FieldAnnot):(U,E)=>V = {
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
      (u,e)=>m.invoke(helper._1,buildParams(Array(u,dst.c,fd,e),permut)).asInstanceOf[V]        
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
      if (!(dst>m.getReturnType)) return false //the return type must be must be acceptable as dst
      val p = m.getParameterTypes
      if (!(src<p(0))) return false            //the first param must be compatible with src
      if (Modifier.isStatic(m.getModifiers) || !(src<m.getDeclaringClass))
        checkParams(p4(p(0)),p,1)!=null        //p(0) has been verified as accepting src ; it could be a super class.
      else
        checkParams(p3,p,1)!=null
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
    if (r==null) None else if (src<in.c) Some(new MethodConverter1(dst,src,r)) else Some(new MethodConverter2(dst,src,r))
  }
}