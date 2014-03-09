package loader.audit

import loader.core.Locator
import loader.core.definition.Processor
import loader.core.events.Event

/** Used to build a log structure.
 */
trait LogRecord {
  def name:String          //the name for the item logged
  def localisation:String  //the line at which the problem is found
  def level:Int            //the level for the log
  def value:Option[Any]    //the value of an object
  def exc:Throwable        //a thrown exception
}

/** Used to register a log */
trait Recorder {
  //severity level that causes a LogRecord to be kept when logging
  def showLevel:Int
  //severity level that causes an exception to be thrown when checking
  def errorLevel:Int
  //returns a function that will build the actual recorder to use, with parameter the previous recorder used (or null)
  def apply(params:Map[String,String]):Recorder
  //logging a LogRecord message in the audit recorder
  def log(lr:LogRecord):Unit
  // possibly logs a LogRecord, provided its level is sufficient
  def log(lvl:Int, lr: =>LogRecord):Unit = if (lvl<=showLevel) log(lr)
  //logging a standard message in the audit recorder
  def log(lvl:Int,loc:String,msg:String):Unit = log(lvl,new LogRecord { val name=""; val level=lvl; val localisation=loc; val value=Some(msg); val exc=null } )
}

/** Used to build Element identifiers
 */
trait IdentifierScheme[-M<:Processor] {
  //builds the name for ld
  def apply(ld:M#GenElt):String
  //builds the name for a child of ld whose simple name is known
  def apply(ld:M#GenElt,name:String):String
}


/** Defines some information on what to log and how.
 */
trait AuditInfo[-M<:Processor] extends PartialFunction[(M#GenElt,Event),LogRecord] {
  def id:IdentifierScheme[M]            //how names are printed
  def max:Int                           //maximal error level logged
}