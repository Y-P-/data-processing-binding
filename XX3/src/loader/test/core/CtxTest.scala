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
  val p = new parsers.Struct(256,40,false)
  val userCtx = new loader.core.UserContext[CtxCore { type BaseParser>:parsers.Struct; type Kind=String }] {
    val buf = new java.io.StringWriter
    override val eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(new PrintWriter(buf))))
    def apply(e:Elt) = new EltContext(e)
    class EltContext(protected[this] val e:Elt) extends super.EltContext(e) {
      override def solver(s:Proc#Kind):()=>Proc#Ret = {
        if (!s.asInstanceOf[String].startsWith("@include:")) return null
        ()=>run.include1[parsers.Struct,Proc](p,e)((u,s)=>s+"*",null,_.read(load("verysmall1"), "UTF-8"))
      }
    }
  }
    
  /** Test to verify that DataActors are correctly found */
  @Test class CtxBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Int = {
      val m = motors.Struct.ctx(out,2,userCtx)
      try {
        run(p,m)(_(ClassContext(classOf[Data.Top])),null,null,_.read(load("small"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
  @Test class CtxCbkTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      val m = motors.Struct.ctx(out,2,userCtx)
      //XXX was implicit to cast callback to callbacks
      try {
        val r:Int = run(p,m)(_(ClassContext(classOf[Data.Top]), DefaultCtxEventsCbk.cbks[motors.Struct.ctx.type](new DefaultCtxEventsCbk[Int,String,String])),null,null,_.read(load("small"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
}