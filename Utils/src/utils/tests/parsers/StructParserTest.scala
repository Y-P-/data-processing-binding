package utils.tests.parsers

import utils.parsers._
import utils.Reader

object StructParserTest {
  
  object parser extends StructParser('{','}','=',';','"','#',256,30,false,'^',Array(('t','\t'),('n','\n'),('u','+'))) with Spy {
    var line=0
    var depth=0
    var l = new Array[String](30)
    def push(ps:State,idx:Int):Unit     = { l(depth)=idx.toString }
    def push(ps:State,name:String):Unit = { l(depth)=name }
    def pull(ps:State,data:String):Unit = { for (j <- 0 to depth) print(if (l(j).length>0) l(j)+"-" else "$-"); println("<"+data+">"); pull(ps)}
    def pull(ps:State):Unit             = { print("<= "); for (j <- 0 to depth) print(if (l(j).length>0) l(j)+"-"); println}
    def err(ps:State,detail:String,cause:String):Nothing = throw new IllegalStateException(s"at <${ps.str}> at line ${ps.line} ($cause at $detail)")
    def handler:PartialFunction[Throwable,Unit] = { case e:Throwable => println(e); throw e; }    
  }

  def main(args:Array[String]) {
    parser(Reader(getClass.getResource("struct1").toURI),parser)
  }  
}
