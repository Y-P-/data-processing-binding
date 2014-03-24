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
  
  /** A minimal class to test most object spawning cases, including collections, deep collections, object nesting
   */
  class Cz {
    import utils.Indent
    @TagField          val id:Int = 0                           //standard scalar field with auto conversion
    @TagField          val ok:Boolean = false                   //same, different conversion
    @TagList           val idA:Array[Double] = null             //array of scalars
    @TagSeq            val idL:List[Integer] = null             //collection of scalars
    @TagList(depth=2)  val id2:Array[Array[Double]] = null      //deep collection of scalars
    
    @TagField(loader=classOf[Cz]) val cz:Cz = null                        //object
    @TagSeq(loader=classOf[Cz])   val czA:Array[Cz] = null                //array of objects
    @TagList(loader=classOf[Cz])  val czL:List[Cz] = null                 //collection of objects
    @TagList(loader=classOf[Cz], depth=2) val cz2:Array[List[Cz]] = null  //deep collection of objects
    
    def p1 = if (idA!=null) s"[${idA.mkString(",")}]" else ""
    def p2 = if (idL!=null) s"$idL" else ""
    def p3 = if (id2!=null) s"[${id2.deep.mkString(",")}]" else ""
    def p4(x:Indent) = if (cz==null) "" else s"${cz.p(x)}"
    def p5(x:Indent):String = if (czA==null) "" else {
      val b = new StringBuffer
      for (v<-czA) b.append(v.p(x))
      b.toString
    }
    def p6(x:Indent):String = if (czL==null) "" else {
      val b = new StringBuffer
      for (v<-czL) b.append(v.p(x))
      b.toString
    }
    def p7(x:Indent):String = if (cz2==null) "" else {
      val b = new StringBuffer
      for (v1<-cz2; v<-v1) b.append(v.p(x))
      b.toString
    }
    def p(x:Indent):String = s"$x$id, ${if(ok)"true, "else""}$p1, ${if(idL!=null) s"$idL, " else ""}$p3${p4(x(2))}${p5(x(2))}${p6(x(2))}${p7(x(2))}"
    override def toString = p(new Indent(0))
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
      println(on)
    }
  }
}
