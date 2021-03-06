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
import loader.core.ExtCore

object ObjTest {
  
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url
  }
  
  abstract class Defaults[P<:ParserBuilder {type Value=String; type Key=String}, M<:ObjectMotor.DefImpl] extends loader.core.UsrCtx[P,M] with ObjectMotor.UCtx[P,M] {
    class EltCtxBase(val elt:Proc#Elt) extends super.EltCtxBase { this:EltCtx=>
      override def solver(s:Proc#Value):()=>Proc#Ret = null
      def keyMap(s:Pars#Key):Proc#Key = s
      def valMap(s:Pars#Value):Proc#Value = s
      override def errHandler(p:Pars#BaseImpl):PartialFunction[Throwable,Unit] = {
        case e => println(e.getMessage)
                  e.printStackTrace
                  super.errHandler(p)
      }
    }
  }
  
  //a generic context that works with any parser for a string processor and ObjectMotor.ctx
  def userCtx(out:PrintWriter) = {
    type P0 = ParserBuilder {type Value=String; type Key=String}
    type M0 = ObjectMotor.ctx.type
    new Defaults[P0,M0] with CtxCore.UsrCtx[P0,M0] {
      override def apply(e:Proc#Elt) = new EltCtx(e)
      class EltCtx(elt:Proc#Elt) extends super[Defaults].EltCtxBase(elt) with super[UsrCtx].EltCtxBase with ObjectMotor.CtxFullInfer[Pars] {
        override def eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(out))) 
      }
    }
  }
  //a generic context that works with any parser for a string processor and ObjectMotor.ext
  def userCtxE(out:PrintWriter) = {
    type P0 = ParserBuilder {type Value=String; type Key=String}
    type M0 = ObjectMotor.ext.type
    new Defaults[P0,M0] {
      type EltCtx = EltCtxBase
      override def apply(e:Proc#Elt) = new EltCtx(e)
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
  /** Test to verify that an object is correctly filled up with ext (other tests with ctx); it tests most cases and ends up with some deep nesting */
  @Test class ObjBaseTotalInferExtTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = {
      import ObjectMotor.ext
      val buf = new java.io.StringWriter
      val on = new CzBase.TotalInferExt
      val m = ext(on)
      val r=run(p,m)(userCtxE(new PrintWriter(buf)),_(),_.read(load("objTestInfer.txt"), "UTF-8"))._2
      out.println(on)
      out.print(buf)
    }
  }
}


//TODO
//Named
//tagEnd (=convert ; influence with inference ?)
//maps
//Cache for binders
//how to manage different contexts for one class ?
//External conf for field kind choice "bsfm"

//Full serialization
//cardinality for inferred lists

//separate @Tag annot with @Check (checking info)
//print exceptions when no log activated
//enable loops for trees; make new tree with no parent but loops possible: loop(split"/")
//define default values for trees

//make Proc/Parser generic in UsrCtx (to avoid cast in ObjDepthTest)
//imagine a generic docking structure for EltCtx into which Tree can fit

//remove def userCtx:UCtx[Builder] from EltBase (we can reach it from eltCtx ; requires different init phase)
//remove Ret in profit of Unit ?
//type checking for multi-interfaces in UCtx
