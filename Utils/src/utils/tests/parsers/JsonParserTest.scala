package utils.tests.parsers

import utils.parsers._
import utils.Reader

object JsonParserTest {
  
  object parser extends JsonParser(256,30,false,true) {
    var ps:State = _
    var l = new Array[String](30)
    def onInit(ps:State)        = this.ps=ps
    def push(idx:Int):Unit      = { l(ps.depth)=idx.toString }
    def push(name:String):Unit  = { l(ps.depth)=name }
    def pull(data:String):Unit  = { for (j <- 1 to ps.depth) print(if (l(j).length>0) l(j)+"-" else "$-"); println(s"<$data> (line: ${ps.line})"); pull() }
    def pull():Unit             = {}
    def err(detail:String,cause:String):Nothing = throw new IllegalStateException(s"at <${ps.str}> at line ${ps.line} ($cause at $detail)")
    def handler:PartialFunction[Throwable,Unit] = { case e:Throwable => println(e); throw e; }    
  }

  def main(args:Array[String]) {
    parser(Reader(getClass.getResource("json1").toURI))
  }  
}