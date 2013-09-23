package loader.core
import loader.core.definition.Def
import loader.core.events.Event

/** Hoelts the framework's defined general exceptions.
 *  They are usually lifted to the package object.
 */
object exceptions {
  
  //index used to easily identify the exception as an Event.
  val userExceptionIdx = 0
  val parseExceptionIdx = 1
  val invalidValueExceptionIdx = 2
  val invalidConversionExceptionIdx = 3
  val includeExceptionIdx = 4
  val degenListExceptionIdx = 5
  val unexpectedExceptionIdx = 6

  /** A user exception that won't be caught by the framework. */
  case class UserException(val lvl:Int,val msg:String,val e:Throwable) extends Exception(msg,e) with Event {
    val idx = userExceptionIdx
  }

  /** A severe error in the framework, unlikely to get caught. It will get out of the framework. */
  class InternalLoaderException(message:String,cause:Throwable,elt:Def#Elt) extends RuntimeException(message,cause) {
    def this(c:Throwable,elt:Def#Elt) = this(null,c,elt)
    def this(m:String,elt:Def#Elt)    = this(m,null,elt)
    def this(elt:Def#Elt)             = this("Illegal state",null,elt)
    def this(m:String)                = this(m,null)
  }
  
  /** Internal, expected, framework problems; these exceptions main purpose is reporting. */
  sealed class LoaderException(val idx:Int) extends Exception with Event {
    override def fillInStackTrace:Throwable = { this }
  }
  
  /** Exception thrown when a parsing error i.e. conversion from string to some value failed */
  case class ParseException(val s:String, val info:String) extends LoaderException(parseExceptionIdx)
  /** Exception thrown when an abnormal value is found ; this can happen either on parsing or on assignation */
  case class InvalidValueException(val v:Any, val info:String) extends LoaderException(invalidValueExceptionIdx)
  /** Exception thrown when a value failed to convert */
  case class InvalidConversionException(val v:String, val e:Throwable) extends LoaderException(invalidConversionExceptionIdx) {
    override def getMessage = v
  }
  /** Exception thrown when an include results in an exception */
  case class IncludeException(val s:String, val e:Throwable) extends LoaderException(includeExceptionIdx) {
    def this(kind:Int,cz1:Class[_],cz2:Class[_]) = this(kind match {
      case 1 => s"parser $cz1 is not supported by processor $cz2"
      case 2 => s"processor $cz1 is not supported by parser $cz2"
      case 3 => s"parser kind $cz1 is not compatible with Top kind $cz2"
    }, null)
  }
  /** Exception thrown when a degenerated list results in an exception */
  case class DegenListException(val s:String, val e:Throwable) extends LoaderException(degenListExceptionIdx)
  /** Exception thrown when an unexpected exception is processed */
  case class UnexpectedException(e:Throwable) extends LoaderException(unexpectedExceptionIdx)

  /** Exceptions used to process dynamic loading (i.e. finding the appropriate FieeltMapping for a child based on the current context) */
  final class DynamicInvocation extends Exception
  final class DynamicConstructor extends Exception

}