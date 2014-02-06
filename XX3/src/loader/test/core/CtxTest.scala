package loader.test.core

import utils.LogTester._
import utils.RegexReplacer
import loader._
import loader.context.ClassContext
import loader.core.definition.Status
import loader.core.callbacks.CallbacksBuilder
import loader.core.names.QName
import loader.features.{DefaultAuditHandler,DefaultCtxEventsCbk,StandardAuditLogger}
import loader.audit.{AuditRecorder,IdScheme}
import java.io.OutputStream
import java.io.PrintWriter
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import loader.test.Data

object CtxTest {
  val userCtx = new loader.core.UserContext[Element { type Kind<:String }] {
    override def eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5))
    override def isInclude(elt:E)(s:elt.Kind):Boolean = false //!elt.isInstanceOf[loader.core.CtxCore.Def#Terminal]
    override def solveInclude(elt:E)(s:elt.Kind) = new loader.parsers.Struct(256,40,false).run(new java.io.File("verysmall1.txt").toURI, "UTF-8")
    override def qName(elt:E) = {
      val q=super.qName(elt)
      if (q!=null) new QName(elt.name.capitalize,q.prefix,q.isAttrib)
      else         new QName(elt.name.capitalize)
    }
  }
  
  /** Test to verify that DataActors are correctly found */
  @Test class CtxBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      val p = new parsers.Struct(256,40,false)
      val m = new motors.Struct.ctx.Motor(out,2,userCtx)
      p.run(getClass.getResource("small").toURI, "UTF-8")(m(ClassContext(classOf[Data.Top])))
    }
  }
  @Test class CtxCbkTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      val p = new parsers.Struct(256,40,false)
      val m = new motors.Struct.ctx.Motor(out,2,userCtx)
      p.run(getClass.getResource("small").toURI, "UTF-8")(m(ClassContext(classOf[Data.Top]), new DefaultCtxEventsCbk[Unit,String]))
    }
  }
}