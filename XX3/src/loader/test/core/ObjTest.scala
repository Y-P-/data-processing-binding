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

object ObjTest {
  
  class Cz {
    @TagField val id:Int = 0
    @TagField val types:Boolean = false
    @TagList  val idx:Array[Double] = null
    @TagList  val idx1:Array[Array[Double]] = null
    @TagField(loader=classOf[Cz]) val cz:Cz = null
    @TagList(loader=classOf[Cz]) val czs:List[Cz] = null
  }
  
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url
  }
  
  //a generic context that works with any parser for a string processor
  def userCtx(out:PrintWriter) = new loader.core.UsrCtx[ParserBuilder {type Value=String; type Key=String},CtxCore {type Value=String; type Key=String}] {self=>
    override def apply(e:Proc#Elt) = eltCtx
    object eltCtx extends super.EltCtx(null) {
      override def eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5,action=AuditRecorder.print(out))) 
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
      val on = new Cz
      val m = ctx(on)
      val r=run(p,m)(userCtx(out),_(ClassContext(classOf[Cz]),DefaultCtxEventsCbk(m)),_.read(load("objTest.txt"), "UTF-8"))._2
      println(on.id) 
      println(on.types)
      println(on.idx(0))
      println(on.idx(1))
      println(on.cz.id)
      println(on.czs(0).id)
      println(on.czs(1).id)
      println(on.czs(2).id)
      println(on.idx1(0)(0))
      println(on.idx1(0)(1))
      println(on.idx1(1)(0))
      println(on.idx1(1)(1))
      println(on.idx1(2)(0))
    }
  }
}
