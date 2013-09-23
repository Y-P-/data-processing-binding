package utils

import java.io.InputStream
import java.io.StringWriter


/**
 * Utility to read from a Stream as it is produced (parallel processing)
 */
object ParallelReader {
  def apply[X,In](is:In)(f:(In)=>X):Either[X,Throwable] = {
    var r:Either[X,Throwable] = null
    val th = new Thread { override def run() = r = try { Left(f(is)) } catch { case e:Throwable => Right(e)} }
    th.start
    th.join
    r
  }
  def noThrow[X](is:InputStream)(f:(InputStream)=>X):X = apply(is)(f) match {
    case Left(s)  => s
    case Right(e) => null.asInstanceOf[X]
  }
  def withThrow[X](is:InputStream)(f:(InputStream)=>X):X = apply(is)(f) match {
    case Left(s)  => s
    case Right(e) => throw e
  }
}


/**
 * Utility to read from a Stream as it is produced (parallel processing) and convert it to string.
 * No fancy encoding.
 */
object StreamToString {
  def apply(is:InputStream):String =
    ParallelReader.withThrow(is) { x => val sw=new StringWriter; var c=0; do {c=x.read; if (c>=0) sw.write(c) } while (c>=0); sw.toString }
  def apply(p:Process):String = apply(p.getInputStream)
}

/**
 * Same as above, reading from a process command output.
 */
object ProcessReader {
  def apply(cmd:String):String = StreamToString(Runtime.getRuntime.exec(cmd))
}
