package utils
import _root_.java.io.{File,FileReader,FileInputStream,BufferedReader,InputStreamReader,ByteArrayOutputStream}
import _root_.java.nio.charset.Charset
import _root_.java.nio.channels.FileChannel
import _root_.java.net.{URL,URI}

object ByteArrayReader {
  
  /**
   * Reads a local file into a buffer.
   */
  def apply(url:URL):Array[Byte] = {
    if (url.getProtocol=="file") { //map the file for speed
      val ch = new FileInputStream(url.toURI.getPath).getChannel
      try {
        val buf = ch.map(FileChannel.MapMode.READ_ONLY, 0, ch.size)
        val bytes = new Array[Byte](ch.size.asInstanceOf[Int])
        buf.get(bytes, 0, bytes.length);
        bytes
      } finally {
        ch.close
      }
    } else { //read the file into an array
      val in = url.openStream
      try {
        val out = new ByteArrayOutputStream
        val buf = new Array[Byte](32768) //use large chunks
        var l = -1
        do { l=in.read(buf); out.write(buf,0,l) } while (l>=0)
        out.toByteArray
      } finally {
        in.close
      }
    }
  }
  def apply(uri:URI):Array[Byte] = apply(uri.toURL)

}