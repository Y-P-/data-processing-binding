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

  import util.matching.Regex

  implicit class RegexContext(val sc: StringContext) extends AnyVal {
    def r(args: Any*):Regex = new Regex(sc.s(args))
    def r:Regex = new Regex(sc.parts(0))
  }
  
  val y="he"
  val u = r"($y.*)"
  def ex(e:Processor#EltBase) = {
    e.names() match {
      case Seq(r"h.*", r"include", _, _*) => println(s"OK")
      case _ => 
    }
  }
  
  //a generic context that works with any parser for a string processor
  val userCtx = new loader.core.UsrCtx[ParserBuilder {type Value=String; type Key=String},CtxCore {type Value=String; type Key=String}] {self=>
    val buf = new java.io.StringWriter
    override def apply(e:Proc#Elt) = new EltCtx(e)
    class EltCtx(protected[this] val e:Proc#Elt) extends super.EltCtx(e) {
      override val eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(new PrintWriter(buf))))
      override def solver(s:Proc#Value):()=>Proc#Ret = {
        if (!s.startsWith("@include:")) return null
        ()=>run.includeX(p,e,false)(self,_.read(load("verysmall1"), "UTF-8"))._2
      }
      def keyMap(s:Pars#Key):Proc#Key = s
      def valMap(s:Pars#Value):Proc#Value = s
    }
  }
  val p = new parsers.Struct(256,40,false)
    
  /** Test to verify that DataActors are correctly found */
  @Test class CtxBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):(Unit,Int) = {
      import motors.Struct.ctx
      val m = ctx(out,2)
      try {
        run(p,m)(userCtx,_(ClassContext(classOf[Data.Top])),_.read(load("small"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
  @Test class CtxCbkTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      import motors.Struct.ctx
      val m = ctx(out,2)
      try {
        run(p,m)(userCtx,_(ClassContext(classOf[Data.Top]),DefaultCtxEventsCbk(m)),_.read(load("small"), "UTF-8"))
      } finally {
        out.print(userCtx.buf)
        userCtx.buf.getBuffer.setLength(0)
      }
    }
  }
}
