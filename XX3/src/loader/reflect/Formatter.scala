package loader.reflect

import scala.reflect.ClassTag
import loader.core.ParserBuilder

/** In some ways, the class that reverses Converters.
 *  It is designed to take an input object, and activate the push/pull operations of a parser to 'write' that object.
 *  That parser must accept String as input.
 *  
 *  There are two broad categories of formatters:
 *  - terminal: the input can be 'written' immediately, with no introspection nor recursion.
 *    This is for example the case for a java.lang.Integer where the sole operation is pull(<integer string representation>)
 *    The object might be more complex, such as a date where one might want to push('year');pull(<year value>);push('month');push(<month value>);...
 *    These usually correspond to similar object in the Converters.
 *  - non terminal: the input must be analyzed in some way, and for each found pair (name,value), the name is pushed and the value checked:
 *    if it can be formatted by a terminal formatter, then that formatter is used. Otherwise we recurse on the non-terminal formatter.
 */
object Formatters {
  import ParserBuilder.Impl

  val noStringConversion = new Exception { override def fillInStackTrace=this }
  
  /** A formatter takes an input X and it manipulates an 'output' parser of kind K.
   *  Most of the time, K will be String.
   */
  trait Formatter[X,K] {
    def apply[P<:Impl[K]](x:X,p:P):Unit
  }

  trait FormatData[K] {
    def fmt:K
  }
  
  abstract class FmtBuilder[X,K] { self=>
    def apply(fd:FormatData[K]):Formatter[X,K]
    def map[Y](f:Y=>X):FmtBuilder[Y,K] = new FmtBuilder[Y,K] {
      def apply(fd:FormatData[K]):Formatter[Y,K] = new Formatter[Y,K] {
        protected[this] val fx = self.apply(fd)
        def apply[P<:Impl[K]](x:Y,p:P):Unit = fx.apply(f(x),p)
      }
    }
  }
  
  /** A basic formatter sitting on the String.format method */
  protected final class XFormatter[X<:AnyRef](fd:FormatData[String],default:String) extends Formatter[X,String] {
    protected[this] val fmt = if (fd.fmt==null || fd.fmt.length==0) default else fd.fmt
    def apply[P<:Impl[String]](x:X,p:P):Unit = p.pull(String.format(fmt,x))
  }
  protected object XFormatter {
    def apply[X<:AnyRef](default:String) = new FmtBuilder[X,String] {
      def apply(fd:FormatData[String]) = new XFormatter[X](fd,"%d")
    }
  }
  /** A basic formatter sitting on the toString method */
  protected final class ZFormatter[X<:AnyRef] extends Formatter[X,String] {
    def apply[P<:Impl[String]](x:X,p:P):Unit = p.pull(x.toString)
  }
  protected object ZFormatter {
    def apply[X<:AnyRef] = new FmtBuilder[X,String] {
      protected[this] val x = new ZFormatter[X]
      def apply(fd:FormatData[String]) = x
    }
  }
  
  val FmtJInt     = XFormatter[Integer]("%d")
  val FmtInt      = FmtJInt.map(new Integer(_:Int))
  val FmtJShort   = XFormatter[java.lang.Short]("%d")
  val FmtShort    = FmtJShort.map(new java.lang.Short(_:Short))
  val FmtJLong    = XFormatter[java.lang.Long]("%d")
  val FmtLong     = FmtJLong.map(new java.lang.Long(_:Long))
  val FmtJByte    = XFormatter[java.lang.Byte]("%d")
  val FmtByte     = FmtJByte.map(new java.lang.Byte(_:Byte))
  val FmtJChar    = XFormatter[java.lang.Character]("%c")
  val FmtChar     = FmtJChar.map(new java.lang.Character(_:Char))
  val FmtJFloat   = XFormatter[java.lang.Float]("%g")
  val FmtFloat    = FmtJFloat.map(new java.lang.Float(_:Float))
  val FmtJDouble  = XFormatter[java.lang.Double]("%d")
  val FmtDouble   = FmtJDouble.map(new java.lang.Double(_:Double))
  
  val FmtBoolean = new FmtBuilder[Boolean,String] {
    protected[this] type X = Boolean
    def apply(fd:FormatData[String]):Formatter[X,String] = new Formatter[X,String] {
      protected[this] val fmt = if (fd.fmt==null || fd.fmt.length==0) Array("true","false") else fd.fmt.split(",")
      def apply[P<:Impl[String]](x:X,p:P):Unit = p.pull(fmt(if (x) 0 else 1))
    }
  }
  val FmtJBoolean  = FmtBoolean.map((_:java.lang.Boolean).booleanValue)
  val FmtURI       = ZFormatter[java.net.URI]
  val FmtURL       = ZFormatter[java.net.URL]
  val FmtPattern   = ZFormatter[java.util.regex.Pattern]
  val FmtDate      = ZFormatter[java.util.Date]
  val FmtFile      = ZFormatter[java.io.File]
  val FmtClass     = ZFormatter[java.lang.Class[_]]
  val FmtJEnum     = ZFormatter[java.lang.Enum[_]]
  val FmtEnum      = ZFormatter[Enumeration#Value]

  /** A formatter that will use an appropriate String conversion defined on a ConvertData */
  def fmtReflect[U](name:String,in:Class[_],src:Class[U]) = new FmtBuilder[U,String] {
    val cv = Converters[U,String,Null](in,src,classOf[String],name).getOrElse(throw noStringConversion)
    def apply(fd:FormatData[String]):Formatter[U,String] = new Formatter[U,String] {
      protected[this] val fmt = cv(ConvertData(fd.fmt))  //no check, no valid
      def apply[P<:Impl[String]](u:U,p:P):Unit = p.pull(fmt(u,null))
    }
  }

}