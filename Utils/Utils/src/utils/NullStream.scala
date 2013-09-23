package utils

import java.io.OutputStream

object NullStream extends OutputStream {
 override def close = ()
 override def flush = ()
 override def write(b:Array[Byte]) = ()
 override def write(b:Array[Byte],off:Int,len:Int) = ()
 def write(b:Int) = ()
}
