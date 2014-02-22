package loader.motors

import java.io.Writer
import loader._
import utils.Indent
import loader.core._
import loader.core.callbacks._
import java.io.FileWriter
import java.io.File
import java.io.OutputStreamWriter
import loader.core.ParserBuilder


object Struct extends Processor {self=>
  
  trait DefImpl extends loader.core.ExtCore.Processor {impl=>
    type Kind        = String
    type Ret         = Int
    type Data        = Null
    type BaseParser  = ParserBuilder //any parser
    
    /**
     * @param out, where to write to
     * @param indent, indent value as space number ; 0 means all output on one line
     */
    abstract class Inner(bp: BaseParser, val out:Writer, val indent:Int=0, userCtx:UserCtx) extends self.Motor(userCtx)
    
    trait MotorImpl {
      val userCtx:UserCtx
      val out:Writer  //where to write to
      val indent:Int  //indent value as space number ; 0 means all output on one line
    
      type Result = Unit
 
      protected def getData(parent:Element,s:Status):Data = null

      private def newLine(e:Element):Unit   = out.write(if (indent>0) Indent((e.depth-1)*indent) else " ")
      private def quote(s:String):Unit      = out.write(if(parser.px.Data.isChar(s)) s else s""""$s"""")
      private def tag(e:Element):Unit       = { newLine(e); if (!e.name.isEmpty) { quote(e.localName); out.write(s" = ") } }
  
      protected def onInit(e:Element):Unit          = {}
      protected def onBeg(e:Element): Unit          = if (e.parent!=null) { tag(e); out.write("{") }
      protected def onVal(e:Element,v: String): Ret = { tag(e); quote(v); out.flush; 1 }
      protected def onEnd(e:Element): Ret           = if (e.parent!=null) { newLine(e); out.write("}"); out.flush; 1 } else 0
      protected def onChild(e:Element,child: Element, r: Ret): Unit = {}

      protected def onInit():Unit = {}
      protected def onExit():Unit = out.flush
    }
  }
  
  protected def readParams(pr: utils.ParamReader) = {
    import utils.StringConverter._
    val append = pr("append", false)(CvBoolean())
    val outFile:File = pr("out", null.asInstanceOf[File])(CvFile("",""))
    val out = if (outFile!=null) new FileWriter(outFile,append) else new OutputStreamWriter(System.out)
    val indent = pr("indent", 2)(CvInt("","",null))
    (out,indent)
  }
  
  object ctx extends loader.core.CtxCore.Impl with DefImpl {
    class Motor(val out:Writer, val indent:Int=0, val userCtx:UserCtx) extends super.Launcher with MotorImpl
    def apply(pr: utils.ParamReader, userCtx:UserCtx):Motor = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
    def apply(out:Writer, indent:Int=0, userCtx:UserCtx) = new Motor(out,indent,userCtx)
  }
  object ext extends loader.core.ExtCore.Impl with DefImpl {
    class Motor(val out:Writer, val indent:Int=0, val userCtx:UserCtx) extends super.Launcher with MotorImpl
    def apply(pr: utils.ParamReader, userCtx:UserCtx) = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
    def apply(out:Writer, indent:Int=0, userCtx:UserCtx) = new Motor(out,indent,userCtx)
  }
  
}

//XXX remove or adapt: clash of names, Motor contravariant
abstract class Processor {
  import loader.core._
  abstract class Motor[P<:definition.Processor](val userCtx:UserContext[P])
  val ctx:CtxCore.Impl
  val ext:ExtCore.Impl
}

