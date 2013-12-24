package loader.reflect

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
  
  type P[-x]    = ParserBuilder { type Kind>:x }
  type Impl[-x] = P[x]#Impl
  
  /** A formatter takes an input X, possibly associated with an Elt of some kind,
   *  and it manipulates an 'output' Elt of another kind.
   */
  trait Formatter[X] {
    def apply[P<:Impl[String]](x:X,p:P):Unit
  }

  trait FormatData {
    def fmt:String
  }
  
  abstract class FmtBuilder[X] { self=>
    def apply(fd:FormatData):Formatter[X]
    def map[Y](f:Y=>X):FmtBuilder[Y] = new FmtBuilder[Y] {
      def apply(fd:FormatData):Formatter[Y] = new Formatter[Y] {
        protected[this] val fx = self.apply(fd)
        def apply[P<:Impl[String]](x:Y,p:P):Unit = fx.apply(f(x),p)
      }
    }
  }
  
  /** A basic formatter sitting on the String.format method */
  protected final class XFormatter[X<:AnyRef](fd:FormatData,default:String) extends Formatter[X] {
    protected[this] val fmt = if (fd.fmt==null || fd.fmt.length==0) default else fd.fmt
    def apply[P<:Impl[String]](x:X,p:P):Unit = p.pull(String.format(fmt,x))
  }
  protected object XFormatter {
    def apply[X<:AnyRef](default:String) = new FmtBuilder[X] {
      def apply(fd:FormatData) = new XFormatter[X](fd,"%d")
    }
  }
  /** A basic formatter sitting on the toString method */
  protected final class ZFormatter[X<:AnyRef] extends Formatter[X] {
    def apply[P<:Impl[String]](x:X,p:P):Unit = p.pull(x.toString)
  }
  protected object ZFormatter {
    def apply[X<:AnyRef] = new FmtBuilder[X] {
      protected[this] val x = new ZFormatter[X]
      def apply(fd:FormatData) = x
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
  
  val FmtBoolean = new FmtBuilder[Boolean] {
    protected[this] type X = Boolean
    def apply(fd:FormatData):Formatter[X] = new Formatter[X] {
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


}