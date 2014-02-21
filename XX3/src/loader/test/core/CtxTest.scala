package loader.test.core

import utils.LogTester._
import utils.RegexReplacer
import loader._
import loader.core.run
import loader.context.ClassContext
import loader.core.CtxCore.Def
import loader.core.definition.Status
import loader.core.callbacks.CallbacksBuilder
import loader.core.names.QName
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

object CtxTest {
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url.toURI
  }
  val p = new parsers.Struct(256,40,false)
  val userCtx = new loader.core.UserContext[Def#Elt { type Kind <: String }] {
    val buf = new java.io.StringWriter
    override val eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(new PrintWriter(buf))))
    def apply(e:Elt) = new EltContext(e)
    class EltContext(protected[this] val e:Elt) extends super.EltContext(e) {
      override def solver(s:K):()=>e.Ret = {
        if (!s.asInstanceOf[String].startsWith("@include:")) return null
     //   return e.incl[String,e.Ret](null,
     //                 p.run(load("verysmall1"), "UTF-8").asInstanceOf[(loader.core.ParserBuilder{type Kind <: String; type BaseProcessor >: loader.core.CtxCore.Def <: loader.core.definition.Def{type Ret <: loader.core.CtxCore.Def#Ret}})#Executor],
     //                 null)
        null
      }
      override def qName = {
        val q=super.qName
        if (q!=null) new QName(e.name.capitalize,q.prefix,q.isAttrib)
        else         new QName(e.name.capitalize)
      }
    }
  }
    
  /** Test to verify that DataActors are correctly found */
  @Test class CtxBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Int = {
      val m = motors.Struct.ctx(out,2,userCtx)
      run(p)(m)(_(ClassContext(classOf[Data.Top])),null,_.read(load("small"), "UTF-8"))
    }
  }
  @Test class CtxCbkTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      userCtx.buf.getBuffer.setLength(0) //reset buffer
      val m = motors.Struct.ctx(out,2,userCtx)
      val r:Int = run(p)(m)(_(ClassContext(classOf[Data.Top]), new DefaultCtxEventsCbk[Int,String]),null,_.read(load("small"), "UTF-8"))
      out.print(userCtx.buf)
    }
  }
}