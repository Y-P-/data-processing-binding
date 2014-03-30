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
import loader.annotations._
import loader.motors.ObjectMotor

object ObjTest {
  
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url
  }
  
  //a generic context that works with any parser for a string processor
  def userCtx(out:PrintWriter) = new ObjectMotor.UCtx[ParserBuilder {type Value=String; type Key=String},ObjectMotor.DefImpl with CtxCore]
                                    with CtxCore.UsrCtx[ParserBuilder {type Value=String; type Key=String},ObjectMotor.DefImpl with CtxCore] {self=>
    override def apply(e:Proc#Elt) = eltCtx
    class EltCtx extends super[UCtx].EltCtxBase with super[UsrCtx].EltCtxBase {
      val elt:Proc#Elt = null
      override def eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(out))) 
      override def solver(s:Proc#Value):()=>Proc#Ret = null
      def keyMap(s:Pars#Key):Proc#Key = s
      def valMap(s:Pars#Value):Proc#Value = s
    }
    val eltCtx = new EltCtx
  }
  val p = new parsers.Struct(256,40,false)
    
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = {
      import ObjectMotor.ctx
      val buf = new java.io.StringWriter
      val on = new CzBase.FullAnnot
      val m = ctx(on)
      val r=run(p,m)(userCtx(new PrintWriter(buf)),_(ClassContext(classOf[CzBase.FullAnnot]),DefaultCtxEventsCbk(m)),_.read(load("objTest.txt"), "UTF-8"))._2
      out.println(on)
      out.print(buf)
    }
  }
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseInferTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = {
      import ObjectMotor.ctx
      val buf = new java.io.StringWriter
      val on = new CzBase.InferAnnot
      val m = ctx(on)
      val r=run(p,m)(userCtx(new PrintWriter(buf)),_(ClassContext(classOf[CzBase.InferAnnot]),DefaultCtxEventsCbk(m)),_.read(load("objTest.txt"), "UTF-8"))._2
      out.println(on)
      out.print(buf)
    }
  }
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseFullInferTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = {
      import ObjectMotor.ctx
      val buf = new java.io.StringWriter
      val on = new CzBase.FullInferAnnot
      val m = ctx(on)
      val r=run(p,m)(userCtx(new PrintWriter(buf)),_(ClassContext(classOf[CzBase.FullInferAnnot]),DefaultCtxEventsCbk(m)),_.read(load("objTest.txt"), "UTF-8"))._2
      out.println(on)
      out.print(buf)
    }
  }
}


//TODO
//Named
//Dynamic
//Default loader
//tagEnd
//maps
//Cache for binders
//External conf for field kind choice "bsfm"

//remove/simplify onChild ? (but do not make Ret=Unit ?)
//finish the ObjectMotor.ext implementation