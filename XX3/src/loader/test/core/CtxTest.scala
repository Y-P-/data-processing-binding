package loader.test.core

import utils.LogTester._
import utils.RegexReplacer
import loader._
import loader.core.run
import loader.context.ClassContext
import loader.core.CtxCore
import loader.core.definition.Status
import loader.core.callbacks.CallbacksBuilder
import loader.core.ParserBuilder
import loader.features.{DefaultAuditHandler,DefaultCtxEventsCbk,StandardAuditLogger}
import loader.audit.{AuditRecorder,IdScheme}
import java.io.OutputStream
import java.io.PrintWriter
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import loader.test.Data
import loader.core.events.EventHandler
import loader.core.definition.Processor

object CtxTest {
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url
  }
  //a generic context that works with any parser for a string processor
  val userCtx = new loader.core.UserContext[ParserBuilder,CtxCore { type BaseParser=ParserBuilder; type Value=String }] {self=>
    val buf = new java.io.StringWriter
    override val eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(new PrintWriter(buf))))
    def apply(e:Elt) = new EltContext(e)
    class EltContext(protected[this] val e:Elt) extends super.EltContext(e) {
      override def solver(s:Proc#Value):()=>Proc#Ret = {
        if (!s.startsWith("@include:")) return null
       // ()=>run.include[p.type,e.proc.type](p,e.myself)(self,(u,s)=>s+"*",null,_.read(load("verysmall1"), "UTF-8"))
        null
      }
    }
  }
  val p = new parsers.Struct(256,40,false)
    
  /** Test to verify that DataActors are correctly found */
  @Test class CtxBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Int = {
      import motors.Struct.ctx
      val m = ctx(out,2)
      try {
        val f = run(p,ctx)(m) _
        f(userCtx,null)
        (userCtx,_(_,ClassContext(classOf[Data.Top]),_)(_.read(load("small"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
  @Test class CtxCbkTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      val m = motors.Struct.ctx(out,2)
      try {
        val r:Int = run(p,m)(userCtx,_(ClassContext(classOf[Data.Top]), DefaultCtxEventsCbk[m.Proc]*),null,null,_.read(load("small"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
}