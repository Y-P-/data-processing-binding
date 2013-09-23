package loader.audit

import scala.collection.mutable.Buffer
import loader.InternalLoaderException

/**
 * This class is used to manage audits on a global scope.
 * This is different from the Auditer interface which only manages one record at a time.
 * @param showLevel for auditing ; any log over that level will not even be built and processed by action
 * @param errorLevel to send an exception to the caller when appropriate
 * @param max number of errors to meet before aborting
 */
class AuditRecorder(val showLevel:Int=2,val errorLevel:Int=2,val max:Int=0,action:(Recorder,LogRecord) => Boolean=AuditRecorder.print) extends Recorder {
  var minLevel:Int = 9                       //min level ever met
  private[this] var minError:LogRecord = _   //min error ever met
  var nb:Int = 0
    
  /** unconditionnaly logs a LogRecord */
  def log(lr:LogRecord):Unit = if (lr!=null) {
    val lvl = lr.level
    if (action(this,lr)) {
      if (lvl<=minLevel)   { if (minLevel!=lvl) minError = lr; minLevel=lvl }
      if (lvl<=errorLevel) {
        nb+=1
        if (nb==max) throw new InternalLoaderException("the loaded data contained fatal errors",null)
      }
    }
  }
    
  def reset():Unit = { minLevel=9; minError=null; nb=0; } 
    
  def apply(params:Map[String,String]):Recorder = {
    val lvl    = params.get("lvl") match {
      case None    => showLevel
      case Some(x) => Integer.decode(x).toInt
    }
    val errLvl    = params.get("err") match {
      case None    => errorLevel
      case Some(x) => Integer.decode(x).toInt
    }
    val maxNb    = params.get("max") match {
      case None    => max
      case Some(x) => Integer.decode(x).toInt
    }
    val kind = params.get("kind") match {
      case None          => action
      case Some("-")     => AuditRecorder.ignore
      case Some("print") => AuditRecorder.print
       case _            => throw new InternalLoaderException("audit kind unknown",null)
    }
    AuditRecorder(lvl,errLvl,maxNb,kind)
  }
}

object AuditRecorder {
  /**
   * A factory that may sometimes provide easier access to simple AuditRecorders.
   * The default factory builds an audit recorder with level 2 and printing.
   */
  def apply(level:Int=2,errorLevel:Int=2,max:Int=0,action:(Recorder,LogRecord) => Boolean=print):Recorder =
    new AuditRecorder(level,errorLevel,max,action)

  final val ignore = (ar:Recorder,lr:LogRecord) => true                //ignore log
  final val print  = (ar:Recorder,lr:LogRecord) => {println(lr);true}  //print log
}