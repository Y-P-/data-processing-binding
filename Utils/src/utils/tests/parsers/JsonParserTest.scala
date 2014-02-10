package utils.tests.parsers

import utils.parsers._
import utils.ByteArrayReader
import utils.LogTester._
import java.io.PrintWriter
import org.junit.Test

class JsonParserTest(data:String) extends StandardTester {
  def apply(file:Solver,out0:PrintWriter) = {
    val parser = new JsonParser(256,30,false,true) with TestHandlerBridge {
      val out = out0
    }
    parser.newProc(ByteArrayReader(file(data)))
  }
}

@Test class JsonParserTest1 extends JsonParserTest("json1")
