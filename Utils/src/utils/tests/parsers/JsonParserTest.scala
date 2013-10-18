package utils.tests.parsers

import utils.parsers._
import utils.Reader

object JsonParserTest {
  
  object parser extends JsonParser(256,30,false,true) with Spy {
    var line=0
    var depth=0
    var l = new Array[String](30)
    def push(ps:State,idx:Int):Unit     = { l(depth)=idx.toString }
    def push(ps:State,name:String):Unit = { l(depth)=name }
    def pull(ps:State,data:String):Unit = { for (j <- 1 to depth) print(if (l(j).length>0) l(j)+"-" else "$-"); println(s"<$data> (line: $line)"); pull(ps) }
    def pull(ps:State):Unit             = {}
    def err(ps:State,detail:String,cause:String):Nothing = throw new IllegalStateException(s"at <${ps.str}> at line ${ps.line} ($cause at $detail)")
    def handler:PartialFunction[Throwable,Unit] = { case e:Throwable => println(e); throw e; }    
  }

  def main(args:Array[String]) {
    parser(Reader(getClass.getResource("json1").toURI),parser)
  }  
}