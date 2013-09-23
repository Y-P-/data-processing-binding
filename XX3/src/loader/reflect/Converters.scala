package loader.reflect

import java.util.regex.Pattern
import java.io.{IOException,File,FileFilter}
import java.net.{URI,URL}
import scala.annotation.tailrec

import validator.BorderFactory
import loader.core.exceptions._

/** Converters are used to handle basic input String.
 *  They are used to convert the String into the expected type.
 *  Some default converters are already implemented ; they are automatically found by introspection.
 *  However, if you need a specific type, you may have to implement your own converter.
 * 
 *  Converters also provide for the ability to validate the input, both before and after conversion.
 *  The standard behaviour is to use a regex to validate the String, a a Validator to check the result.
 *  These may not always offer what may be required, and it is also possible to override either or both methods.
 */

//Beware about null (human default) vs "" (machine default)
//FIXME put converters in factories

abstract class Converter[S] {
  /** Border factory if any
   */
  def bf:BorderFactory[S] = null
  /** Action done to validate the input String.
   *  If a ParseException is thrown, it should contain s as first parameter
   *  It must throw an exception if the input is not valid.
   *  By default, we use regex.
   *  @return the actual string that will get converted ; usually the same as input
   */
  def validStr(valider:String):(String)=>String = Converter.validStr(valider)
  /** Actual conversion from String to the expected ouput.
   */
  @throws(classOf[ParseException])
  @throws(classOf[InvalidConversionException])
  def read(fmt:String,data:String):S
  /** Appropriate formatting of S.
   *  A null or empty fmt should write a String that can be natively read with no error by read with a null or empty fmt.
   *  By default, we assume that toString works well.
   */
  def format(fmt:String,s:S):String = s.toString
  /** Parametrized converter. It doesn't always exist.
   */
  def withParam(p:String):Converter[S] = this
  /** Standard validation using the BorderFactory
   */
  def valid(valider:String):(S)=>S = { val v=Converter.valid[S](valider); if (v!=null) v(this) else identity }
}

object Converter {
  /** default method to valid string entries */
  def validStr(valider:String):(String)=>String = if (valider!=null && !valider.isEmpty) {
    val  p = Pattern.compile(valider)
    (s:String) => { if (!p.matcher(s).matches) throw new ParseException(s,s"regex was $valider"); s }
  } else identity[String] 
  /** default method to valid item conversions using BorderFactories */
  def valid[S](valider:String) = if (valider==null || valider.isEmpty) { (cv:Converter[S]) => identity[S] _ } else
    (cv:Converter[S]) => if (cv.bf==null) null else {
      val checker = cv.bf.Intervals(valider,cv.read(null,_))  //reminder: null format is for default human-readable
      (s:S) => { if (!checker.check(s)) throw new InvalidValueException(s," acceptable values are "+valider); s }
    }
  def apply[S](cv:Converter[S],fmt:String,input:String,validResult:(S)=>S):S = {
    try {
      var r=cv.read(fmt,input)
      if (validResult!=null)
        r=validResult(r)
      r
    } catch {
      case e:LoaderException => throw e
      case x:Throwable       => throw new InvalidConversionException(input,x)
    }    
  }
  object CvInt extends Converter[Int] {
    override val bf = BorderFactory[Int](_-_,_-_==1)
    override def format(fmt:String,data:Int) = fmt match {
      case "" | "d" | null => Integer.toString(data)
      case "b"             => Integer.toBinaryString(data)
      case "h"             => Integer.toHexString(data)
      case "o"             => Integer.toOctalString(data)
    }
    def read(fmt:String,data:String):Int = Integer.decode(data)
  }
  object CvLong extends Converter[Long] {
    override def format(fmt:String,data:Long) = fmt match {
      case "" | "d" | null => java.lang.Long.toString(data)
      case "b"             => java.lang.Long.toBinaryString(data)
      case "h"             => java.lang.Long.toHexString(data)
      case "o"             => java.lang.Long.toOctalString(data)
    }
    override val bf            = BorderFactory[Long]((x,y)=>if (x>y) 1 else if (x<y) -1 else 0,_-_==1)
    def read(fmt:String,data:String):Long = java.lang.Long.decode(data)
  }
  object CvShort extends Converter[Short] {
    override val bf                            = BorderFactory[Short](_-_,_-_==1)
    override def format(fmt:String,data:Short) = CvInt.format(fmt,data.asInstanceOf[Short])
    def read(fmt:String,data:String):Short     = java.lang.Short.decode(data)
  }
  object CvByte extends Converter[Byte] {
    override val bf                           = BorderFactory[Byte](_-_,_-_==1)
    override def format(fmt:String,data:Byte) = CvInt.format(fmt,data.asInstanceOf[Short])
    def read(fmt:String,data:String):Byte     = java.lang.Byte.decode(data)
  }
  object CvFloat extends Converter[Float] {
    override val bf = BorderFactory[Float]((x,y)=>if (x>y) 1 else if (x<y) -1 else 0,null)
    override def format(fmt:String,data:Float) = fmt match {
      case "raw"     => CvInt.format(null,java.lang.Float.floatToRawIntBits(data))
      case "" | null => java.lang.Float.toString(data)
      case x         => String.format(x, new java.lang.Float(data))
    }
    def read(fmt:String,data:String):Float = java.lang.Float.parseFloat(data)
  }
  object CvDouble extends Converter[Double] {
    override val bf = BorderFactory[Double]((x,y)=>if (x>y) 1 else if (x<y) -1 else 0,null)
    override def format(fmt:String,data:Double) = fmt match {
      case "raw"     => CvLong.format(null,java.lang.Double.doubleToRawLongBits(data))
      case "" | null => java.lang.Double.toString(data)
      case x         => String.format(x, new java.lang.Double(data))
    }
    def read(fmt:String,data:String):Double = java.lang.Double.parseDouble(data)
  }
  object CvChar extends Converter[Char] {
    override val bf = BorderFactory[Char](_-_,_-_==1)
    def read(fmt:String,data:String):Char = data(0)
  }
  object CvCharArray extends Converter[Array[Char]] {
    def read(fmt:String,data:String):Array[Char] = data.toCharArray
  }
  object CvString extends Converter[String] {
    def read(fmt:String,data:String):String = data
    override def format(fmt:String,s:String) = s
  }
  object CvBoolean extends Converter[Boolean] {
    val vrai = Set("yes","oui","vrai","true","1","y","o","v","t")
    val faux = Set("no","non","faux","false","0","n","f")
    def read(fmt:String,data:String):Boolean = {
      val d = data.toLowerCase
      if      (vrai.contains(d)) true
      else if (faux.contains(d)) false
      else                       throw new java.text.ParseException(data,0)
    }
  }
  object CvDate extends Converter[java.util.Date] {
    val base = new java.text.SimpleDateFormat
    override val bf = BorderFactory[java.util.Date](_.compareTo(_),null)
    override def format(fmt:String,v:java.util.Date) = fmt match {
      case "" | null => base.format(v)
      case x         => new java.text.SimpleDateFormat(x).format(x)
    }
    def read(fmt:String,data:String):java.util.Date = base.parse(data)
  }
  object CvClass extends Converter[Class[_]] {
    def read(fmt:String,data:String):Class[_]  = Class.forName(data)
    override def format(fmt:String,v:Class[_]) = v.getName
  }
  object CvURL extends Converter[URL] {
    def read(fmt:String,data:String):URL = new URL(data)
    override def valid(valider:String):(URL)=>URL = if (valider!=null && valider=="c") {
                                                      (url)=> { url.openStream.close; url } //Will throw an IOException if ressource not available
                                                    } else null
  }
  object CvURI extends Converter[URI] {
    def read(fmt:String,data:String):URI = new URI(data)
  }
  class CvEnum[X<:Enum[_]](val clzz:Class[X]) extends Converter[X] {
    val m = clzz.getMethod("valueOf", classOf[String])
    def read(fmt:String,data:String):X = m.invoke(null,data).asInstanceOf[X]
  }
  object CvEnum extends ((Class[_])=>CvEnum[_]) {
    def apply(c:Class[_]):CvEnum[_] = new CvEnum(c.asSubclass(classOf[Enum[_]]))
  }
  /** Will not work on basic Enums. Requires that the actual value class is defined in the Enum object. See Input, Output classes */
  class CvEnumeration[X<:Enumeration#Value](val clzz:Class[X]) extends Converter[X] {
    val cz = clzz.getEnclosingClass
    val m  = cz.getMethod("withName", classOf[String])
    def read(fmt:String,data:String):X = m.invoke(null,data).asInstanceOf[X]
  }
  object CvEnumeration extends ((Class[_])=>CvEnumeration[_]) {
    def apply(c:Class[_]):CvEnumeration[_] = new CvEnumeration(c.asSubclass(classOf[Enumeration#Value]))
  }
  object CvFile extends Converter[File] {
    def read(fmt:String,data:String):File   = new File(data)
    override def format(fmt:String,file:File):String = file.getAbsolutePath
    /** Possible options are: (f|d|F|D)(r|R|w|W|c)*
     *  - f: path indicates a file ; if it exists, check that it refers to a file
     *  - F: id. f ; if creation required, intermediate path created too if needed.
     *  - d: path indicates a directory ; if it exists, check that it refers to a file
     *  - D: id. d ; if creation required, intermediate path created too if needed.
     *  - r: checks that file/directory exists and can be read
     *  - R: create file/directory if it doesn't exist
     *  - w: checks that file/directory exists and can be writen to
     *  - W: create file/directory if it doesn't exist, truncate it if it exists (for directories : empties it)
     *  - c: checks that file/directory doesn't exist and create it
     *  All checks are done in the order they appear.
     *  No option will do nothing and just create the File object without any check.
     *  Ex: "Fc" will create a file on the path (including intermediate directories), and throw an error if the file already exists.
     *      "Fr+" will do the same with no error, or check that the file indeed exists (but still throw an error if it is not a file)
     */
    override def valid(valider:String):(File)=>File = {
      if (valider==null || valider.isEmpty) return null
      val p1 = java.util.regex.Pattern.compile("([fFdD])([rRwWc]*)")
      val p2 = java.util.regex.Pattern.compile("([rRwWc])(.*)")    
      @tailrec def break(l:List[String],str:String):List[String] = { //breaks the options into first part/remaining part
        if (str==null || str.length==0) return l
        val m2 = p2.matcher(str)
        m2.matches
        return break(m2.group(1)::l,m2.group(2))
      }
      val (dir,path,opt) = try {
        val m1 = p1.matcher(valider)
        m1.matches
        val g1 = m1.group(1)
        (g1=="d"||g1=="D", g1=="F"||g1=="D",break(List(),m1.group(2)).reverse)
      } catch {
        case e:Throwable=>throw new IllegalArgumentException(valider+" is not accepted as arguments for validation in the CvFile converter")
      }
      def delete(f:File):Unit = if (f.isFile) f.delete else { //recursive delete
        for (fc <- f.listFiles(null.asInstanceOf[FileFilter])) delete(fc)
        if (f.list.length==0) f.delete else throw new IOException("failed to delete a directory : "+f)
      }
      (f:File) => {
        def create() = try { //will create the file
          val b = if (path) { if (dir) f.mkdirs else { f.getParentFile.mkdirs; f.createNewFile } }
                  else      { if (dir) f.mkdir  else f.createNewFile }
          if (!b) throw new InvalidValueException(f," could not be created")
        } catch {
          case e:Throwable => throw new InvalidValueException(f," could not be created : "+e.getMessage)
        }
        def checkExists(b:Boolean) = if (f.exists!=b) { //checks the filme existence
          if (f.exists) throw new InvalidValueException(f," already exists")
          else          throw new InvalidValueException(f," does not exist")
        }
        if (f.exists) { //Check correct type if exists
          if (f.isDirectory && !dir) throw new InvalidValueException(f," path exists and is a directory (file requested)")
          if (!f.isDirectory && dir) throw new InvalidValueException(f," path exists and is a file (directory requested)")
        }
        for (o <- opt) o match {
          case "r" => checkExists(true); if (!f.canRead) throw new InvalidValueException(f," cannot be read")
          case "R" => if (!f.exists) create
          case "w" => checkExists(true); if (!f.canWrite) throw new InvalidValueException(f," cannot be written to")
          case "W" => if (!f.exists) create else if (f.isFile) { f.delete; f.createNewFile } else { delete(f); create }
          case "c" => checkExists(true); create
        }
        f
      }
    }
  }

  /** Default known converters.
   */
  protected final val defaults = Map[Class[_],Converter[_<:Any]](
    classOf[String]              -> CvString,
    classOf[Array[Char]]         -> CvCharArray,
    classOf[Int]                 -> CvInt, 
    classOf[Long]                -> CvLong, 
    classOf[Double]              -> CvDouble,
    classOf[Short]               -> CvShort,
    classOf[Byte]                -> CvByte,
    classOf[Char]                -> CvChar,
    classOf[Boolean]             -> CvBoolean,
    classOf[Float]               -> CvFloat,
    classOf[java.lang.Integer]   -> CvInt,
    classOf[java.lang.Long]      -> CvLong,
    classOf[java.lang.Double]    -> CvDouble,
    classOf[java.lang.Short]     -> CvShort,
    classOf[java.lang.Byte]      -> CvByte,
    classOf[java.lang.Character] -> CvChar,
    classOf[java.lang.Boolean]   -> CvBoolean,
    classOf[java.lang.Float]     -> CvFloat,
    classOf[URI]                 -> CvURI,
    classOf[URL]                 -> CvURL,
    classOf[File]                -> CvFile,
    classOf[java.util.Date]      -> CvDate,
    classOf[Class[_]]            -> CvClass
  )
  protected final val classDefaults = utils.ClassMap[(Class[_])=>Converter[_]] (
    classOf[Enum[_]]           -> CvEnum,
    classOf[Enumeration#Value] -> CvEnumeration
  )
  /** Checks whether a converter exists for class clzz.
   */
  def canConvert(clzz:Class[_]) = clzz!=null && (defaults.getOrElse(clzz,null)!=null || classDefaults.get(clzz)!=None)
  /** Gets a converter from the requested class to the required class
   */
  def get[R](clzz:Class[_]):Converter[R] = defaults.getOrElse(clzz,null) match {
    case null => classDefaults.get(clzz) match {
                   case None => null
                   case cv   => cv.get(clzz).asInstanceOf[Converter[R]]
                 }
    case cv   => cv.asInstanceOf[Converter[R]]
  }
}

