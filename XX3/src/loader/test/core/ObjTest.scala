package loader.test.core

import utils.LogTester._
import utils.stringContextes._
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

object ObjTest {
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url
  }
  
  //a generic context that works with any parser for a string processor
  val userCtx = new loader.core.UsrCtx[ParserBuilder {type Value=String; type Key=String},CtxCore {type Value=String; type Key=String}] {self=>
    val buf = new java.io.StringWriter
    val eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(new PrintWriter(buf))))
    override def apply(e:Proc#Elt) = eltCtx
    object eltCtx extends super.EltCtx(null) {
      override def eventHandler = self.eventHandler 
      override def solver(s:Proc#Value):()=>Proc#Ret = null
      def keyMap(s:Pars#Key):Proc#Key = s
      def valMap(s:Pars#Value):Proc#Value = s
    }
  }
  val p = new parsers.Struct(256,40,false)
    
  /** Test to verify that DataActors are correctly found */
  @Test class ObjBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = {
      import loader.motors.ObjectMotor.ctx
      val m = ctx(new Data.Id)
      try {
        run(p,m)(userCtx,_(ClassContext(classOf[Data.Id])),_.read(load("id.txt"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
}
