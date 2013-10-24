package utils

import java.io.OutputStream
import java.io.Writer

final object tee {
  def apply(out1:OutputStream,out2:OutputStream):OutputStream = new OutputStream {
    def write(i:Int):Unit = { if (out1!=null) out1.write(i); if (out2!=null) out2.write(i) }
  }
  def apply(out1:Writer,out2:Writer):Writer = new Writer {
    def write(s:Array[Char], i1:Int, i2:Int) = { if (out1!=null) out1.write(s,i1,i2); if (out2!=null) out2.write(s,i1,i2) }
    def flush() = { if (out1!=null) out1.flush(); if (out2!=null) out2.flush() }
    def close() = { try { if (out1!=null) out1.close() } finally { if (out2!=null) out2.close() } }
  }
}