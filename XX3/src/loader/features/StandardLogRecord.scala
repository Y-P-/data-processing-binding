package loader.features
import java.io.{PrintWriter,ByteArrayOutputStream}
import loader.audit.LogRecord

/** A 'standard' log record which describes the error.
 */
abstract class StandardLogRecord(val name:String,val level:Int,val localisation:String,val value:Option[Any],val exc:Throwable) extends LogRecord {
  def cause:String
  def explain:String
  def printExc  = false
  def excTrace  = if (exc!=null && printExc) { val b = new ByteArrayOutputStream; val p = new PrintWriter(b,true); exc.printStackTrace(p); b.toString } else ""
  override def toString = {
    val v1 = s"$cause $level [$localisation] $name"
    val v3 = explain match {
      case "" =>  ""
      case s  => s" : $s"
    }
    val v4 = excTrace match {
      case "" =>  ""
      case s  => s"\n$s"
    }
    s"$v1$v3$v4"
  }
}
