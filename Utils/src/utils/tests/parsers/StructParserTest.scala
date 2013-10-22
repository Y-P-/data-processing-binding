package utils.tests.parsers

import utils.parsers._
import utils.Reader

object StructParserTest {
  
  object parser extends StructParser('{','}','=',';','"','#',256,30,false,'^',Array(('t','\t'),('n','\n'),('u','+'))) {
    var ps:State = _
    var l = new Array[String](30)
    def onInit(ps:State)                        = this.ps=ps
    def push(idx:Int):Unit                      = { l(ps.depth)=idx.toString }
    def push(name:String):Unit                  = { l(ps.depth)=name }
    def pull(data:String):Unit                  = { for (j <- 0 to ps.depth) print(if (l(j).length>0) l(j)+"-" else "$-"); println("<"+data+">"); pull()}
    def pull():Unit                             = { print("<= "); for (j <- 0 to ps.depth) print(if (l(j).length>0) l(j)+"-"); println}
    def err(detail:String,cause:String):Nothing = throw new IllegalStateException(s"at <${ps.str}> at line ${ps.line} ($cause at $detail)")
    def handler:PartialFunction[Throwable,Unit] = { case e:Throwable => println(e); throw e; }    
  }

  def main(args:Array[String]) {
    parser(Reader(getClass.getResource("struct1").toURI))
  }  
}
