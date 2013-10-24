package utils.tests.parsers

import utils.parsers.State
import java.io.PrintWriter

trait TestHandlerBridge extends utils.parsers.Handler {
    val out:PrintWriter
    import out.{print,println}
    var ps:State = _
    var l = new Array[String](30)
    l(0)=""
    def printStack()            = for (j <- 0 to ps.depth) print(if (l(j).length>0) l(j)+"-" else ".-")
    def onInit(ps:State)        = this.ps=ps
    def push(idx:Int):Unit      = { l(ps.depth)=idx.toString; print("=> "); printStack; println }
    def push(name:String):Unit  = { l(ps.depth)=name; print("=> "); printStack; println }
    def pull(data:String):Unit  = { printStack; println(s"<$data> (line: ${ps.line})"); pull() }
    def pull():Unit             = { print("<= "); printStack; println }
    def err(detail:String,cause:String):Nothing = throw new IllegalStateException(s"at <${ps.str}> at line ${ps.line} ($cause at $detail)")
    def handler:PartialFunction[Throwable,Unit] = { case e:Throwable => println(e); throw e; }    
}