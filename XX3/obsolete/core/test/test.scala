package loader.core.test

import loader.parsers.Struct
import java.io.File
import loader._

/** A minimal test to check that all abilities of the implemention are kept.
 *  Most notably, we are interested by the interoperability of derived classes.
 *  Check Print, Print1, Print2, Print3 as examples of how one can create a processor.
 */
object test {
  import scala.language.implicitConversions

  def main(args:Array[String]):Unit = {
    //creating a top element that will actually work on another kind of processor
    val m = new Print
    val m1 = new Print3
    val top = m(new o.Status(""))
    
    val q = new Struct
    val r = q.run(new File("SMALL").toURI, "UTF-8")(top)
  } 
  
}
