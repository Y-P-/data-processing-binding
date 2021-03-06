package utils

import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.reflect.ClassTag
import validator.BorderFactory
import java.text.ParseException
import java.util.regex.Pattern
import validator.Formatter
import utils.reflect.Reflect.RichClass
import java.lang.reflect.InvocationTargetException


/** Defines a conversion from U to V using a parameter as a guide for conversion.
 */
trait Converter[-U<:AnyRef,+V] {
  type Info  //a data structure used to guide the conversion
  def src:Class[_>:U]  //the maximal class accepted by that converter
  def dst:Class[_<:V]  //the minimal class returned by the converter
  def apply(u:U,i:Info):V
}

/** Conversions from String are especially common and important.
 *  This class is abstract because the inner helper class, Info, is unknown.
 */
abstract class StringConverter[+S:ClassTag] extends Converter[String,S] {
  final protected[this] type T = S   //for easy access inside
  type Info <: String=>S
  def src = classOf[String]
  val dst:Class[_<:S] = implicitly[ClassTag[S]].runtimeClass.asInstanceOf[Class[_<:S]]
  def apply(s:String,i:Info):S = i(s)
}

object StringConverter {
  /** For String, Info will do all the job and contain the whole logic.
   */
  abstract class BasicInfo[S] extends ((String)=>S) {
    @throws(classOf[ParseException])
    def prevalid(s:String):String  //string prevalidation (syntactic check)
    @throws(classOf[IllegalArgumentException])
    def postvalid(s:S):S           //value postvalidation (semantic check)
    def convert(s:String):S        //base conversion process
    @throws(classOf[ParseException])
    @throws(classOf[IllegalArgumentException])
    def apply(s:String):S = postvalid(convert(prevalid(s)))
  }
  abstract class OrderedInfo[S] extends BasicInfo[S] {
    val checker:BorderFactory[S]#Intervals
    def postvalid(s:S):S = {
      if (checker!=null && !checker.check(s)) throw new IllegalArgumentException(s" acceptable values are $checker")
      s
    }
  }
  trait RegexChecker {
    val pattern:Pattern
    def prevalid(s:String):String = {
      if (pattern==null || pattern.matcher(s).matches) s
      else throw new ParseException(s,0)
    }
  }
  protected abstract class InfoBorderBuilder[S](implicit ct: ClassTag[S]) extends StringConverter[S] {
    protected val cvBorder:(String)=>S
    implicit protected def toChecker(s:String)(implicit bf:BorderFactory[S], f:Formatter):BorderFactory[S]#Intervals = if (s==null || s.isEmpty) null else BorderFactory.check(s,cvBorder)
  }
  implicit protected def toRegex(s:String):Pattern = if (s==null || s.isEmpty) null else Pattern.compile(s)
  
  /**********************************  STANDARD CONVERTERS ****************************************/
  //note: you never 'have to' use them. They're just convenient.
  
  object CvInt extends InfoBorderBuilder[Int] {
    import java.lang.Integer.{parseInt=>parse,decode}
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern, radix:Int) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = if (radix>1) parse(s,radix) else decode(s)
    }
    val cvBorder = parse(_:String)
    implicit def toConvert(s:String):Int = s match { case null|"" => 0; case x => Integer.parseInt(x) }
    def apply(validInfo:String,fmtRegex:String,convertInfo:String) = new Info(validInfo,fmtRegex,convertInfo)
  }
  object CvJInt extends StringConverter[java.lang.Integer] {
    def apply(validInfo:String,fmtRegex:String,convertInfo:String):String=>T = CvInt(validInfo,fmtRegex,convertInfo)(_)
  }
  object CvLong extends InfoBorderBuilder[Long] {
    import CvInt.toConvert
    import java.lang.Long.{parseLong=>parse,decode}
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern, radix:Int) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = if (radix>1) parse(s,radix) else decode(s)
    }
    val cvBorder = parse(_:String)
    def apply(validInfo:String,fmtRegex:String,convertInfo:String) = new Info(validInfo,fmtRegex,convertInfo)
  }
  object CvJLong extends StringConverter[java.lang.Long] {
    def apply(validInfo:String,fmtRegex:String,convertInfo:String):String=>T = CvLong(validInfo,fmtRegex,convertInfo)(_)
  }
  object CvShort extends InfoBorderBuilder[Short] {
    import CvInt.toConvert
    import java.lang.Short.{parseShort=>parse,decode}
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern, radix:Int) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = if (radix>1) parse(s,radix) else decode(s)
    }
    val cvBorder = parse(_:String)
    def apply(validInfo:String,fmtRegex:String,convertInfo:String) = new Info(validInfo,fmtRegex,convertInfo)
  }
  object CvJShort extends StringConverter[java.lang.Short] {
    def apply(validInfo:String,fmtRegex:String,convertInfo:String):String=>T = CvShort(validInfo,fmtRegex,convertInfo)(_)
  }
  object CvByte extends InfoBorderBuilder[Byte] {
    import CvInt.toConvert
    import java.lang.Byte.{parseByte=>parse,decode}
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern, radix:Int) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = if (radix>1) parse(s,radix) else decode(s)
    }
    val cvBorder = parse(_:String)
    def apply(validInfo:String,fmtRegex:String,convertInfo:String) = new Info(validInfo,fmtRegex,convertInfo)
  }
  object CvJByte extends StringConverter[java.lang.Byte] {
    def apply(validInfo:String,fmtRegex:String,convertInfo:String):String=>T = CvByte(validInfo,fmtRegex,convertInfo)(_)
  }
  object CvFloat extends InfoBorderBuilder[Float] {
    import java.lang.Float.{parseFloat=>parse}
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = parse(s)
    }
    val cvBorder = parse(_:String)
    def apply(validInfo:String,fmtRegex:String) = new Info(validInfo,fmtRegex)
  }
  object CvJFloat extends StringConverter[java.lang.Float] {
    def apply(validInfo:String,fmtRegex:String):String=>T = CvFloat(validInfo,fmtRegex)(_)
  }
  object CvDouble extends InfoBorderBuilder[Double] {
    import java.lang.Double.{parseDouble=>parse}
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = parse(s)
    }
    val cvBorder = parse(_:String)
    def apply(validInfo:String,fmtRegex:String) = new Info(validInfo,fmtRegex)
  }
  object CvJDouble extends StringConverter[java.lang.Double] {
    def apply(validInfo:String,fmtRegex:String):String=>T = CvDouble(validInfo,fmtRegex)(_)
  }
  object CvChar extends InfoBorderBuilder[Char] {
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = if (s.length!=1) throw new IllegalArgumentException(s"expecting string of exactly one character: <$s> found") else s.charAt(0)
    }
    val cvBorder = (_:String).charAt(0)
    def apply(validInfo:String,fmtRegex:String) = new Info(validInfo,fmtRegex)
  }
  object CvJChar extends StringConverter[java.lang.Character] {
    def apply(validInfo:String,fmtRegex:String):String=>T = CvChar(validInfo,fmtRegex)(_)
  }
  object CvBoolean extends StringConverter[Boolean] {
    class Info(vrai:Pattern,faux:Pattern) extends BasicInfo[T] {
      def convert(s:String):T =  {
        val d = s.toLowerCase
        if      (vrai.matcher(d).matches) true
        else if (faux.matcher(d).matches) false
        else                              throw new java.text.ParseException(s,0)
      }
      def postvalid(s:Boolean) = s
      def prevalid(s:String)   = s
    }
    def apply(vrai:String="yes|oui|vrai|true|1|y|o|v|t", faux:String="no|non|faux|false|0|n|f") = new Info(vrai,faux)
  }
  object CvJBoolean extends StringConverter[java.lang.Boolean] {
    def apply(vrai:String,faux:String):String=>T = CvBoolean(vrai,faux)(_)
  }
  object CvString extends StringConverter[String] {
    class Info(val pattern:Pattern) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T  = s
      def postvalid(s:T) = s
    }
    def apply(fmtRegex:String) = new Info(fmtRegex)
  }
  object CvCharArray extends StringConverter[Array[Char]] {
    class Info(val pattern:Pattern) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T  = s.toCharArray
      def postvalid(s:T) = s
    }
    def apply(fmtRegex:String) = new Info(fmtRegex)
  }
  object CvDate extends InfoBorderBuilder[java.util.Date] {
    class Info(val checker:BorderFactory[T]#Intervals,val pattern:Pattern,sdf:java.text.DateFormat) extends OrderedInfo[T] with RegexChecker {
      def convert(s:String):T = sdf.parse(s)
    }
    val cvBorder = new java.text.SimpleDateFormat().parse(_:String)
    def apply(validInfo:String,fmtRegex:String,convertInfo:String) = new Info(validInfo,fmtRegex,if (convertInfo!=null && !convertInfo.isEmpty) new java.text.SimpleDateFormat(convertInfo) else new java.text.SimpleDateFormat)
  }
  object CvClass extends StringConverter[Class[_]] {
    class Info(val pattern:Pattern,top:Class[_]) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T  = Class.forName(s)
      def postvalid(s:T) = if (top!=null) s.asSubclass(top) else s
    }
    def apply(validInfo:String,fmtRegex:String) = new Info(fmtRegex,if (validInfo!=null && validInfo.length>0) Class.forName(validInfo) else null)
  }
  object CvURL extends StringConverter[java.net.URL] {
    class Info(val pattern:Pattern,checkAccessible:Boolean) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T = new java.net.URL(s)
      def postvalid(s:T) = { if (checkAccessible) s.openStream.close; s } //throws IOException if fails
    }
    def apply(fmtRegex:String,checkAccessible:Boolean) = new Info(fmtRegex,checkAccessible)
  }
  implicit object CvURI extends StringConverter[java.net.URI] {
    class Info(val pattern:Pattern) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T = new java.net.URI(s)
      def postvalid(s:T) = s
    }
    def apply(fmtRegex:String) = new Info(fmtRegex)
  }
  implicit object CvFile extends StringConverter[java.io.File] {
    /** Files controls are sophisticated.
     *  You can :
     *  - check if a file exists (+ toggle) or doesn't exist (- toggle)
     *  - create/replace the file/directory (c toggle)
     *    note: +c will only truncate an existing file or empty a directory (error if the object doesn't exist)
     *    -c will create a new file or directory if it doesn't exist (error if it exists)
     *    c will leave you in all cases with an empty file or directory
     *    C (capital letter) will also create intermediate directories if necessary.
     *  - check that you are working on a file (f toggle or d toggle) (error if the object exists and is not the expected kind)
     *  - check that you have read or write access (r and w toggles)
     */
    class Info(val pattern:Pattern,checkExists:Boolean,checkDoesntExist:Boolean,isFile:Boolean,isDir:Boolean,create:Boolean,createIntermediate:Boolean,hasRead:Boolean,hasWrite:Boolean) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T = new java.io.File(s)
      def postvalid(s:T) = {
        if (checkExists && !s.exists)              throw new IllegalArgumentException(s"$s doesn't exist")
        if (checkDoesntExist && s.exists)          throw new IllegalArgumentException(s"$s already exists")
        if (isFile && s.exists && s.isDirectory)   throw new IllegalArgumentException(s"$s is a directory. A file is expected")
        if (isDir && s.exists && s.isFile)         throw new IllegalArgumentException(s"$s is a file. A directory is expected")
        if (create && s.exists && s.isFile)        s.delete()
        if (create && s.exists && s.isDirectory)   { delete(s); s.mkdir() }
        if (create && !(try {
                          if (isFile)              { if (createIntermediate) s.getParentFile.mkdirs(); s.createNewFile() }
                          else if (isDir)          { if (createIntermediate) s.mkdirs() else s.mkdir() }
                          else false
                        } catch { case _:Exception => false })
           ) throw new IllegalArgumentException(s"could not create $s")
        if (hasRead && !s.canRead)                 throw new IllegalArgumentException(s"$s cannot be read")
        if (hasWrite && !s.canWrite)               throw new IllegalArgumentException(s"$s cannot be written to")
        s
      }
      def delete(f:java.io.File):Unit = if (f.isFile) f.delete else { //recursive delete
        for (fc <- f.listFiles(null.asInstanceOf[java.io.FileFilter])) delete(fc)
        if (f.list.length==0) f.delete else throw new java.io.IOException("failed to delete a directory : "+f)
      }
    }
    def apply(validInfo:String,fmtRegex:String) = new Info(
        fmtRegex,
        validInfo.contains('+'),
        validInfo.contains('-'),
        validInfo.contains('f'),
        validInfo.contains('d'),
        validInfo.contains('c')||validInfo.contains('C'),
        validInfo.contains('C'),
        validInfo.contains('r'),
        validInfo.contains('w')
        )
  }
  
  class CvEnum[X<:Enum[_]:ClassTag] extends StringConverter[X] {
    val m = dst.c.getMethod("valueOf", classOf[String])
    m.setAccessible(true)
    class Info(val pattern:Pattern) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T = {
        val r = try { m.invoke(null,s) } catch { case x:InvocationTargetException => throw x.getCause }
        dst.c.cast(r)
      }
      def postvalid(s:T) = s
    }
    def apply(fmtRegex:String) = new Info(fmtRegex)
  }
  /* Note: fragile code ; subject to scala compiler implementation changes.
   * Cannot work if the inner class Val is not overloaded, because it is protected thus cannot be directly referenced.
   * Any declaration such as val e:X.Value (where X inherits from Enumerated) thus get translated to val e:Enumeration#Value.
   * The actual container type (X) is lost in the process and cannot be retrieved by reflection.
   * Furthermore, we require that the path be stable (otherwise unknown instances are required to parse the enumeration.)
   * 
   * A minimal working implementation is:
   *   object X extends Enumeration {
   *     class Val protected[X] extends super.Val
   *     val e0=new Val
   *     val e1=new Val
   *   }
   * This works because class Val is now public and knows about it's declaring class.
   */
  class CvEnumeration[X<:Enumeration#Value:ClassTag] extends StringConverter[X] {
    // Will not work on basic Enumerations. Requires that the actual Value class is defined in the Enum object.
    // Note: scala reflection is obscure, and debugging did not let me find the enclosing class.
    // We rely on a (very) dirty hack.
    val withName:String=>Any = {
      val name = dst.c.getName
      val end  = name.lastIndexOf('$')
      val base = name.substring(end)
      val x = if (base.indexOf('.')<0) //class name ends with XXX$YYY => the enclosing class is XXX
                try   { Class.forName(name.substring(0, end)+"$") }
                catch { case _:Throwable => throw new IllegalArgumentException(s"Enumeration ${dst.c} must be stable; reflection will fail.") }
              else
                throw new IllegalArgumentException(s"Enumeration ${dst.c} is not precise enough; reflection will fail.")
      val m = x.getMethod("withName", classOf[String])
      m.setAccessible(true)
      val o = x.asObject
      if (!Modifier.isStatic(m.getModifiers) && o==null)
        throw new IllegalArgumentException(s"it is not possible to find an instance to invoke $m")
      s => try {
        m.invoke(o,s)
      } catch {
        case e:InvocationTargetException => e.getCause match {
          case e:NoSuchElementException => throw new IllegalArgumentException(s"No element named $s exists for Enumeration $name")
          case e => throw e
        }
      }
    }
    class Info(val pattern:Pattern) extends BasicInfo[T] with RegexChecker {
      def convert(s:String):T = dst.c.cast(withName(s))
      def postvalid(s:T) = s
    }
    def apply(fmtRegex:String) = new Info(fmtRegex)
  }
}