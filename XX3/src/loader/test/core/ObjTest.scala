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
  def userCtx(out:PrintWriter) = new ObjectMotor.UCtx[ParserBuilder {type Value=String; type Key=String},ObjectMotor.ctx.type]
                                   with CtxCore.UsrCtx[ParserBuilder {type Value=String; type Key=String},ObjectMotor.ctx.type] {self=>
    override def apply(e:Proc#Elt) = new EltCtx(e)
    class EltCtx(val elt:Proc#Elt) extends super[UCtx].EltCtxBase with super[UsrCtx].EltCtxBase with ObjectMotor.CtxFullInfer[Pars] {
      override def eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(out))) 
      override def solver(s:Proc#Value):()=>Proc#Ret = null
      def keyMap(s:Pars#Key):Proc#Key = s
      def valMap(s:Pars#Value):Proc#Value = s
    }
  }
  val p = new parsers.Struct(256,40,false)
  
  def baseTest(out:PrintWriter,on:AnyRef) = {
    import ObjectMotor.ctx
    val buf = new java.io.StringWriter
    val m = ctx(on)
    val r=run(p,m)(userCtx(new PrintWriter(buf)),_(ClassContext(on.getClass),DefaultCtxEventsCbk(m)),_.read(load("objTest.txt"), "UTF-8"))._2
    out.println(on)
    out.print(buf)
  }
    
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = baseTest(out,new CzBase.FullAnnot)
  }
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseInferTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = baseTest(out,new CzBase.InferAnnot)
  }
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseFullInferTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = baseTest(out,new CzBase.FullInferAnnot)
  }
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseTotalInferTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = baseTest(out,new CzBase.TotalInferAnnot)
  }
}


//TODO
//Named
//tagEnd (=convert ; influence with inference ?)
//maps
//Cache for binders
//External conf for field kind choice "bsfm"
//Full serialization
//cardinality for inferred lists: see below
//separate @Tag annot with @Check (checking info)
//how to manage different contexts for one class ?

//remove def userCtx:UCtx[Builder] from EltBase (contained in eltCtx ; requires different init phase)
//remove Ret in profit of Unit ?
//finish the ObjectMotor.ext implementation
//type checking for multi-interfaces in UCtx

//Testing:
//Dynamic (use userCtx.eltCtx.onName) => test = full serialization
