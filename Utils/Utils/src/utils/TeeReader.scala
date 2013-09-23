package utils

import java.io.{InputStream,OutputStream}

final class TeeInputStream(val in:InputStream,val out:OutputStream) extends InputStream {
  override def available     = in.available
  override def close         = in.close
  override def mark(r:Int)   = in.mark(r)
  override def markSupported = in.markSupported
  def read                   = in.read
  override def reset         = in.reset
  def skip(n:Int)            = in.skip(n)
}