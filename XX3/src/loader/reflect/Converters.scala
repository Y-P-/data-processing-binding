package loader.reflect

import scala.reflect.ClassTag
import utils.StringConverter._
import loader.core.definition.Def
import utils.Reflect._
import java.lang.reflect.Constructor
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import loader.annotations.Convert

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
  def apply(fd:ConvertData):(U,E)=>V
}

trait ConvertData {
  def check:String   //info for checking the validity of the parser data (Parser.Kind)
  def valid:String   //info for checking the validity of the processor data (Processor.Kind)
  def param:String   //info for transforming the data within the processor  
}
object ConvertData {
  val empty = new ConvertData {
    val check = ""
    val valid = ""
    val param = ""
  }
}

/** This object defines the usual default conversions.
 */
object Converters {
  
  abstract class StringConverter[+V](val dst:RichClass[_<:V]) extends Converter[String,V,Def#Elt] {
    def this(s:utils.StringConverter[V]) = this(s.dst)
    val src = ^(classOf[String])
  }
  object StringConverter {
    implicit def toX[R](cz:StringConverter[R]):utils.ClassMap.Factory[_,StringConverter[_]] = new utils.ClassMap.Factory[R,StringConverter[R]] {
      def build[Y <: R](c: Class[Y]): StringConverter[R] = cz
      def max: RichClass[R] = cz.dst
    }
  }
  trait StringConverterBuilder[V] {
    def apply(c:Class[V]):StringConverter[V]
  }
  
  protected[this] implicit def toConverter[S](f:String=>S) = (s:String,e:Def#Elt) => f(s)
  protected[this] implicit def toMapElt[U:ClassTag](sc:StringConverter[U]) = (implicitly[ClassTag[U]].runtimeClass,sc)
  
  //bridge from ConvertData to standard converters. 
  object CvvString extends StringConverter(CvString) {
    def apply(fd:ConvertData) = CvString(fd.check)
  }
  object CvvCharArray extends StringConverter(CvCharArray) {
    def apply(fd:ConvertData) = CvCharArray(fd.check)
  }
  object CvvInt extends StringConverter(CvInt) {
    def apply(fd:ConvertData) = CvInt(fd.valid,fd.check,fd.param)
  }
  object CvvJInt extends StringConverter(CvJInt) {
    def apply(fd:ConvertData) = CvJInt(fd.valid,fd.check,fd.param)
  }
  object CvvLong extends StringConverter(CvLong) {
    def apply(fd:ConvertData) = CvLong(fd.valid,fd.check,fd.param)
  }
  object CvvJLong extends StringConverter(CvJLong) {
    def apply(fd:ConvertData) = CvJLong(fd.valid,fd.check,fd.param)
  }
  object CvvShort extends StringConverter(CvShort) {
    def apply(fd:ConvertData) = CvShort(fd.valid,fd.check,fd.param)
  }
  object CvvJShort extends StringConverter(CvJShort) {
    def apply(fd:ConvertData) = CvJShort(fd.valid,fd.check,fd.param)
  }
  object CvvByte extends StringConverter(CvByte) {
    def apply(fd:ConvertData) = CvByte(fd.valid,fd.check,fd.param)
  }
  object CvvJByte extends StringConverter(CvJByte) {
    def apply(fd:ConvertData) = CvJByte(fd.valid,fd.check,fd.param)
  }
  object CvvChar extends StringConverter(CvChar) {
    def apply(fd:ConvertData) = CvChar(fd.valid,fd.check)
  }
  object CvvJChar extends StringConverter(CvJChar) {
    def apply(fd:ConvertData) = CvJChar(fd.valid,fd.check)
  }
  object CvvFloat extends StringConverter(CvFloat) {
    def apply(fd:ConvertData) = CvFloat(fd.valid,fd.check)
  }
  object CvvJFloat extends StringConverter(CvJFloat) {
    def apply(fd:ConvertData) = CvJFloat(fd.valid,fd.check)
  }
  object CvvDouble extends StringConverter(CvDouble) {
    def apply(fd:ConvertData) = CvDouble(fd.valid,fd.check)
  }
  object CvvJDouble extends StringConverter(CvJDouble) {
    def apply(fd:ConvertData) = CvJDouble(fd.valid,fd.check)
  }
  object CvvBoolean extends StringConverter(CvBoolean) {
    def apply(fd:ConvertData) = {
      val x = if (fd.param==null || fd.param.length==0) "yes|oui|vrai|true|1|y|o|v|t@no|non|faux|false|0|n|f" else fd.param
      val s=x.split("@")
      CvBoolean(s(0),s(1))
    }
  }
  object CvvJBoolean extends StringConverter(CvJBoolean) {
    def apply(fd:ConvertData) = {
      val x = if (fd.param==null || fd.param.length==0) "yes|oui|vrai|true|1|y|o|v|t@no|non|faux|false|0|n|f" else fd.param
      val s=x.split("@")
      CvJBoolean(s(0),s(1))
    }
  }
  object CvvPattern extends StringConverter[java.util.regex.Pattern](classOf[java.util.regex.Pattern]) {
    def apply(fd:ConvertData) = (s:String,e:Def#Elt) => java.util.regex.Pattern.compile(s)
  }  
  object CvvURL extends StringConverter(CvURL) {
    def apply(fd:ConvertData) = CvURL(fd.check,fd.valid=="C")
  }
  object CvvURI extends StringConverter(CvURI) {
    def apply(fd:ConvertData) = CvURI(fd.check)
  }
  object CvvDate extends StringConverter(CvDate) {
    def apply(fd:ConvertData) = CvDate(fd.valid,fd.check,fd.param)
  }
  object CvvFile extends StringConverter(CvFile) {
    def apply(fd:ConvertData) = CvFile(fd.valid,fd.check)
  }
  object CvvClass extends StringConverter(CvClass) {
    def apply(fd:ConvertData) = CvClass(fd.valid,fd.check)
  }
  
  object CvvEnum extends utils.ClassMap.Factory[Enumeration#Value,StringConverter[_]] {
    val max = ^(classOf[Enumeration#Value])
    def build[X<:Enumeration#Value](cz:Class[X]) = new StringConverter[X](cz) {
      def apply(fd:ConvertData) = new CvEnumeration[X]()(ClassTag(cz))(fd.check)
    }
  }
  object CvvJEnum extends utils.ClassMap.Factory[Enum[_],StringConverter[_]] {
    //Note that Enum[_] is generic, but abstract. Concrete classes (i.e. java enums) are not generic, and these will be effectively
    //tested. So in a way the "no generics" contract is not broken here.
    val max = ^(classOf[Enum[_]])
    def build[X<:Enum[_]](cz:Class[X]) = new StringConverter[X](cz) {
      def apply(fd:ConvertData) = new CvEnum[X]()(ClassTag(cz))(fd.check)
    }
  }
  
 
  //the default ClassMap for string converters.
  val defaultMap = utils.ClassMap[StringConverter[_]](
                        CvvString,CvvCharArray,CvvInt,CvvJInt,CvvShort,CvvJShort,CvvLong,CvvJLong,
                        CvvByte,CvvJByte,CvvChar,CvvJChar,CvvFloat,CvvJFloat,CvvDouble,CvvJDouble,
                        CvvURL,CvvURI,CvvDate,CvvFile,CvvBoolean,CvvJBoolean,CvvClass,CvvPattern,
                        CvvEnum,CvvJEnum
                        )
 
  protected trait MethodConverter {
    def m:Method
  }
  //m is a method with no param, or one or two params in (ConvertData,Def#Elt) (in this order)
  //it belongs to class U
  final protected class MethodConverter1[-U<:AnyRef,+V,-E<:Def#Elt] private (p:Array[Int], val m:Method, val src:RichClass[_>:U], val dst:RichClass[_<:V]) extends Converter[U,V,E] with MethodConverter {
    def apply(fd:ConvertData):(U,E)=>V = {
      (u,e)=>m.invoke(u,buildParams(Array(fd,e),p):_*).asInstanceOf[V]
    }
    override def toString:String = s"MethodConverter1 with $m"
  }
  protected object MethodConverter1 {
    def apply[U<:AnyRef,V,E<:Def#Elt](p:Array[Int], m:Method, src:RichClass[_>:U], dst:RichClass[_<:V]) = if (p==null) null else new MethodConverter1[U,V,E](p,m,src,dst)
  }
  //m is a method with one to three params in (U, ConvertData, Def#Elt) (in this order, U being mandatory)
  //m can be either static, or belong to some class which is not U, of which one instance will be spawned in order to serve for the invocation
  final protected class MethodConverter2[U<:AnyRef,+V,-E<:Def#Elt](in:RichClass[_],p:Array[Int], val m:Method, val src:RichClass[_>:U], val dst:RichClass[_<:V]) extends Converter[U,V,E] with MethodConverter {
    def apply(fd:ConvertData):(U,E)=>V = {
      val helper = if (Modifier.isStatic(m.getModifiers)) {
        //static: find a matching method on the 3 params (src,ConvertData,Def#Elt), src being first and mandatory
        (null,null)
      } else {
        if (in.asObject != null) (in.asObject,null)              //return a singleton object
        else in.findConstructor(Array(czConvertData),0) match {  //instance: find a matching constructor on possibly 1 param (ConvertData)
          case x if x.length>1  => throw new IllegalStateException(s"too many constructor match the accepted entries for ${m.getDeclaringClass}") 
          case x if x.length==0 => throw new IllegalStateException(s"no constructor match the accepted entries for ${m.getDeclaringClass}") 
          case x                => (x(0)._1.newInstance(buildParams(Array(fd),x(0)._2):_*),x(0)._2) //create an appropriate helper class with the right params
        }
      }
      //wrong, was suppoed to detect and forbid invocations which contained fd if fd is used in the constructor ; seems unecessarily restricive. And code is wrong (compares indexes!)
      //if (helper._2!=null) for (x <- helper._2) if (p.contains(x)) throw new IllegalStateException(s"method $m cannot be used to convert (is used both in constructor and method)")
      (u,e)=>m.invoke(helper._1,buildParams(Array(u,fd,e),p):_*).asInstanceOf[V]        
    }
    override def toString:String = s"MethodConverter2 with $m"
  }
  protected object MethodConverter2 {
    def apply[U<:AnyRef,V,E<:Def#Elt](in:RichClass[_], p:Array[Int], m:Method, src:RichClass[_>:U], dst:RichClass[_<:V]) = if (p==null) null else new MethodConverter2[U,V,E](in,p,m,src,dst)
  }
  private[this] val czConvertData:RichClass[_] = classOf[ConvertData]
  
  //finds a method in class 'in' that is an appropriate converter from U to V. If none satisfy, null is returned.
  //bridge or synthetic methods are rejected (i.e. we accept only user defined methods)
  //a Convert eligible method takes precedence over any other method
  //if a name is specified, only methods with that name are examined, otherwise (null or "") all method are examined and the "best" match kept.
  def apply[U<:AnyRef,V,E<:Def#Elt:ClassTag](in:RichClass[_], src:RichClass[U], dst:RichClass[V], name:String):Option[Converter[U,V,E]] = {
    val czE:RichClass[_] = implicitly[ClassTag[E]].runtimeClass
    val isSrc = src<in
    //Returns a converter for a method that satisfies the constraints for being used for conversion to V, None otherwise
    def check(m:Method):Option[(Array[Int],Method)] = {
      if (name!=null && name.length>0 && m.getName!=name) return None
      if (!(dst>m.getReturnType)) return None //the return type must be must be acceptable as dst
      val a:Array[RichClass[_]] = if (isSrc) Array(czConvertData,czE) else Array(src,czConvertData,czE)
      val cp = checkParams(m.getParameterTypes,a,if (isSrc) 0 else 1)
      if (cp==null) None else Option((cp,m))
    }
    try {
      val min = utils.Reflect.AccessibleElement.min(in.methods.flatMap(m=>check(m)))(_._2)
      Some(if (isSrc) MethodConverter1[U,V,E](min._1,min._2,src,dst) else MethodConverter2[U,V,E](in,min._1,min._2,src,dst))
    } catch {
      case utils.Reflect.AccessibleElement.nonComparable => None
    }
  }
}