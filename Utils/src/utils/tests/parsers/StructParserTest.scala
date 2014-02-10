package utils.tests.parsers

import utils.parsers._
import utils.ByteArrayReader
import utils.LogTester._
import java.io.PrintWriter
import org.junit.Test

class StructParserTest(data:String) extends StandardTester {
  def apply(file:Solver,out0:PrintWriter) = {
    val parser = new StructParser('{','}','=',';','"','#',256,30,false,'^',Array(('t','\t'),('n','\n'),('u','+'))) with TestHandlerBridge {
      val out = out0
    }
    parser.newProc(ByteArrayReader(file(data)))
  }
}

@Test class StructParserTest1 extends StructParserTest("struct1")

object StructParserTest {
  def main(args:Array[String]) = (new StructParserTest1).test
}
