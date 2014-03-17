package loader.features

import loader.core.definition._
import loader.core.events._
import loader.core.exceptions._
import loader.audit.{AuditInfo,IdentifierScheme,LogRecord,AuditRecorder}
import loader.core.events.Event.{DispatchByClassArray,Dispatcher}


class StandardAuditLogger[-P<:Processor](val id:IdentifierScheme[P], val max:Int) extends AuditInfo[P] {
  import Event._
  protected[this] final type E = P#Elt
  protected[this] final type Logger[+Evt<:Event] = Dispatcher[P,Evt,(Int)=>LogRecord]
  protected[this] def logDispatcher[Evt<:Event:Manifest](a:Array[_<:Logger[Evt]],severity:Array[((E,Event))=>Int]):PartialFunction[(E,Event),LogRecord] = {
    val d = new DispatchByClassArray[P,Evt,(Int)=>LogRecord](a)
    return { case x:(E,Event) if d.isDefinedAt(x) => { val s=severity(x._2.idx)(x); if (s<=max) d(x)(s) else null } }
  }
  /** Handled base events */
  protected[this] def baseLoggers =  Array(
          ReadTagLog,
          FastWarnLog,
          FastDisabledLog,
          DefaultValueLog,
          InvalidCardinalityLog,
          OutOfSeqLog,
          IllegalRepetitionLog,
          TagNotFoundLog,
          IgnoredTagLog,
          IncludeSuccessLog
          )
  /** Handled exception events */
  protected[this] def exceptionLoggers = Array(
          UserLog,
          ParseLog,
          InvalidValueLog,
          InvalidConversionLog,
          IncludeLog,
          DegenListLog,
          UnexpectedExceptionLog,
          StackExceptionLog
          )
  /**
   * Severities for events. The severity serves as a filter and an event whose severity is greater than max will get past the guard
   * (so that it won't go down the handler chain), but it won't do anything. Lower values are more severe.
   * Usually severities are constant, but is is perfectly possible to dynamically set the severity from either the associated element
   * or from the underlying event. This lets fine tune logging as much as may be needed.
   */
  protected[this] def baseSeverity      = Array[((E,Event))=>Int](_=>5, _=>5, _=>5, _=>4, _=>4, _=>4, _=>4, _=>4, _=>4, _=>4)
  protected[this] def exceptionSeverity = Array[((E,Event))=>Int](_._2.asInstanceOf[UserException].lvl, _=>3, _=>3, _=>3, _=>3, _=>3, _=>2, _=>2)
  
  private[this] val t1 = logDispatcher(baseLoggers,baseSeverity)
  private[this] val t2 = logDispatcher(exceptionLoggers,exceptionSeverity)
  
  val log:PartialFunction[(E,Event),LogRecord] = t1 orElse t2
  
  def localisation(elt:E):String = {
    val b = new StringBuilder
    elt.foreach { u:E=>             //for (u:E <- elt) :  compiler crash
      if      (u.parent==null) b.append(u.parser.location)
      else if (u.isInclude)    b.append("/").append(u.parser.location)
    }
    b.toString
  }
  
  abstract private class Log1(x:(E,Event),name:String,lvl:Int,v:Option[Any],exc:Throwable)
    extends StandardLogRecord(id(x._1,name),lvl, localisation(x._1),v,exc) {
    override def printExc = lvl<=2
  }
  
  private object StackExceptionLog extends Logger[StackException] {
    def process(x:(E,StackException)) = new Log1(x,null,_,None,x._2) {
      val explain = s"parser stack underflow"
      val cause   = "X"
    }
  }
  private object UnexpectedExceptionLog extends Logger[UnexpectedException] {
    def process(x:(E,UnexpectedException)) = new Log1(x,null,_,None,x._2.e) {
      val explain = s"unexpected error : ${exc.getClass}${if (exc.getMessage!=null) s" (${exc.getMessage})" else ""}"
      val cause   = "X"
    }
  }
  private object ParseLog extends Logger[ParseException] {
    def process(x:(E,ParseException)) = new Log1(x,null,_,Some(x._2.s),x._2) {
      val explain = s"could not be parsed : ${x._2.info}"
      val cause   = "Xp"
    }
  }
  private object InvalidValueLog extends Logger[InvalidValueException] {
    def process(x:(E,InvalidValueException)) = new Log1(x,null,_,Some(x._2.v),x._2) {
      val explain = s"value was not accepted : ${x._2.info}"
      val cause   = "Xv"
    }
  }
  private object InvalidConversionLog extends Logger[InvalidConversionException] {
    def process(x:(E,InvalidConversionException)) = new Log1(x,null,_,None,x._2.e) {
      val explain = s"value ${x._2.v} could not be converted : ${x._2.getMessage}"
      val cause   = "Xc"
    }
  }
  private object IncludeLog extends Logger[IncludeException] {
    def process(x:(E,IncludeException)) = new Log1(x,null,_,Some(x._2.s),x._2.e) {
      val explain = s"degenerated list form couldn't be handled; check that the relevant solver name is declared in the factory : "
      val cause   = "Xi"
    }
  }
  private object DegenListLog extends Logger[DegenListException] {
    def process(x:(E,DegenListException)) = new Log1(x,null,_,Some(x._2.s),x._2.e) {
      val explain = s"degenerated list form couldn't be handled; check that the relevant solver name is declared in the factory : "
      val cause   = "Xl"
    }
  }
  private object UserLog extends Logger[UserException] {
    def process(x:(E,UserException)) = new StandardLogRecord(id(x._1,null),_,localisation(x._1),None,x._2.e) {
      val explain = x._2.msg
      val cause   = "U "
    }
  }
  private object ReadTagLog extends Logger[ReadTagEvt[Any]] {
    def process(x:(E,ReadTagEvt[Any])) = new Log1(x, null, _, Some(x._2.s), null) {
      val explain = s"${if (x._2.s!=null) s"'${x._2.s}' " else ""}was read${if (x._2.v.isInstanceOf[Unit]) "" else s": (${x._2.v})"}"
      val cause   = "R "
    }
  }
  private object FastWarnLog extends Logger[FastWarnEvt] {
    def process(x:(E,FastWarnEvt)) = new Log1(x, null, _, None, null) {
      val explain = s"fast skipping will prevent out of sequence detection"
      val cause   = "f+"
    }
  }
  private object FastDisabledLog extends Logger[FastDisabledEvt] {
    def process(x:(E,FastDisabledEvt)) = new Log1(x,null, _, None, null) {
      val explain = s"fast skipping was overridden by non contiguous sequence"
      val cause   =  "f-"
    }
  }
  private object DefaultValueLog extends Logger[DefaultValueEvt] {
    def process(x:(E,DefaultValueEvt)) = new Log1(x, x._2.name, _, None, null) {
      val explain = "was set to it's default value"
      val cause   =  "D "
    }
  }
  private object InvalidCardinalityLog extends Logger[InvalidCardinalityEvt] {
    def process(x:(E,InvalidCardinalityEvt)) = new Log1(x, x._2.name, _, None, null) {
      val explain = s"cardinality check failed - ${if(x._2.min>0) s"${x._2.min} min, " else ""}${if(x._2.max>0) s"${x._2.max} max, " else ""}${x._2.nb} found"
      val cause   =  "C "
    }
  }
  private object OutOfSeqLog extends Logger[OutOfSeqEvt] {
    def process(x:(E,OutOfSeqEvt)) = new Log1(x, x._2.name, _, None, null) {
      val explain = s"was found out of sequence (only first such item in a sub-sequence shown)"
      val cause   =  "s "
    }
  }
  private object IllegalRepetitionLog extends Logger[IllegalRepetitionEvt] {
    def process(x:(E,IllegalRepetitionEvt)) = new Log1(x, x._2.name, _, None, null) {
      val explain = s"illegal repetition of a simple field"
      val cause   =  "C "
    }
  }
  private object TagNotFoundLog extends Logger[TagNotFoundEvt] {
    def process(x:(E,TagNotFoundEvt)) = new Log1(x, x._2.name, _, None, null) {
      val explain = s"mandatory tag not found - ${x._2.min} expected"
      val cause   =  "C "
    }
  }
  private object IgnoredTagLog extends Logger[IgnoredTagEvt] {
    def process(x:(E,IgnoredTagEvt)) = new Log1(x, x._2.name, _, None, null) {
      val explain = "was ignored"
      val cause   =  "I "
    }
  }
  private object IncludeSuccessLog extends Logger[IncludeSuccessEvt[Any]] {
    def process(x:(E,IncludeSuccessEvt[Any])) = new Log1(x, null, _, Some(x._2.info), null) {
      val explain = s"include successfull"
      val cause   =  "i "
    }
  }

  def isDefinedAt(x:(E,Event)):Boolean = log.isDefinedAt(x)
  def apply(x:(E,Event)):LogRecord     = log.apply(x)
}

/* XXX new events to handle ?
  def unrecognizedParam(nm:String,value:String):LogRecord = log(XunrecognizedParam,null) { new Log1(_,_,Some(value)) { //XXX useless ?
    def explain = s"unrecognized parameter ${nm} = ${value}"
  }}
  def invalidValueTypeLog(ld:E,v:Any,e:Throwable):LogRecord = log(XinvalidValueTypeLog,ld) { new Log1(_,_,Some(v),e) {
    def explain = s"could not be assigned : ${e.getMessage}"
  }}
  def parserEnd(ld:E,e:Throwable):LogRecord = log(XparserEnd,ld) { new Log1(_,_,exc=e) {
    def explain = s"parsing ended ${if (e==null) "successfully" else s"in error : $e"}"
  }}  
  def parserErrorLog(ld:E,e:Throwable):LogRecord = log(XparserErrorLog,ld) { new Log1(_,_,exc=e) {
    def explain = s"parser failed : $e"
  }}
*/

/** Event handler for auditing.
 */
class DefaultAuditHandler[-P<:Processor](a:AuditInfo[P],ar:AuditRecorder) extends EventHandler[P] {
  def isDefinedAt(x:(P#Elt,Event)) = a.isDefinedAt(x)
  def apply(x:(P#Elt,Event)):Unit = ar.log(a(x))
}
