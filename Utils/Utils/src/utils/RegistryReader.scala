package utils

import java.io.{InputStream,StringWriter}
import scala.util.control.Breaks._

object RegistryReader {
  val marks = Array("REG_SZ","REG_DWORD")
  
  /**
   * Holds the content of a whole key, preventing having to rerun the process in that case.
   * Use the next utility if you retrieve only one value from a key.
   */
  class Key(key:String) {
    val read = try {
      ProcessReader("reg query \""+key+"\"")
    } catch {
      case _:Throwable => null
    }
    /** gets a specific value in that key */
    def apply(value:String):String = {
      val i = read.indexOf(value)
      if (i<0) return null
      val s = read.substring(i+value.length+1, read.length)
      val j = s.indexOf('\n')
      analyze(if (j>0) s.substring(0,j) else s)
    }
  }
  
  /** Analyzes s (exerpt from reg query result) to isolate the data part */
  protected def analyze(s:String):String = {
    for (tk <- marks) { val p=s.indexOf(tk); if (p>=0) return s.substring(p + tk.length).trim }
    null
  }
  
  /**
   * Gets a specific value in a key.
   * Use the previous utility if you retrieve more than one value from a key.
   */
  def apply(key:String,value:String):String = analyze(ProcessReader("reg query \""+key+"\" /v \""+value+"\""))
}

