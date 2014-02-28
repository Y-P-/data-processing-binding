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
import loader.core.definition._

object Struct {self=>
  
  trait DefImpl {
    type Element <: Processor#Element
    type Status
    type Kind       = String
    type Key        = String
    type Ret        = Int
    type Data       = Null
    type BaseParser = ParserBuilder //any parser
    
    def getData(parent:Element,s:Status):Data = null
    val noKey = ""
  }
  
  /**
   * @param out, where to write to
   * @param indent, indent value as space number ; 0 means all output on one line
   */    
  class Impl[Status,UserCtx](val userCtx:UserCtx, val out:Writer, val indent:Int) extends Delegate[Processor#Element,String,String,Status,UserCtx,Int] {
    type Result = Unit
    
    private def newLine(e:Elt):Unit   = out.write(if (indent>0) Indent((e.depth-1)*indent) else " ")
    private def quote(s:String):Unit  = out.write(if(parser.px.Data.isChar(s)) s else s""""$s"""")
    private def tag(e:Elt):Unit       = { newLine(e); if (!e.name.isEmpty) { quote(e.name); out.write(s" = ") } }
  
    def onInit(e:Elt):Unit          = {}
    def onBeg(e:Elt): Unit          = if (e.parent!=null) { tag(e); out.write("{") }
    def onVal(e:Elt,v: String): Int = { tag(e); quote(v); out.flush; 1 }
    def onEnd(e:Elt): Int           = if (e.parent!=null) { newLine(e); out.write("}"); out.flush; 1 } else 0
    def onChild(e:Elt,child: Elt, r: Int): Unit = {}

    def onInit():Unit = {}
    def onExit():Unit = out.flush
  }
  
  protected def readParams(pr: utils.ParamReader) = {
    import utils.StringConverter._
    val append = pr("append", false)(CvBoolean())
    val outFile:File = pr("out", null.asInstanceOf[File])(CvFile("",""))
    val out = if (outFile!=null) new FileWriter(outFile,append) else new OutputStreamWriter(System.out)
    val indent = pr("indent", 2)(CvInt("","",null))
    (out,indent)
  }
  
  /** Actual implementations. Due to the nature of this processor, we have all three implementations available.
   */
  object ctx extends loader.core.CtxCore.Abstract[Null] with DefImpl {
    override type Data = Null
    class Motor(val out:Writer, val indent:Int=0, val userCtx:UserCtx) extends super.Motor {
      override val delegate = new Impl(userCtx,out,indent)
    }
    def apply(pr: utils.ParamReader, userCtx:UserCtx):Motor = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
    def apply(out:Writer, indent:Int=0, userCtx:UserCtx) = new Motor(out,indent,userCtx)
  }
  object ext extends loader.core.ExtCore.Abstract[Null] with DefImpl {
    override type Data = Null
    class Motor(val out:Writer, val indent:Int=0, val userCtx:UserCtx) extends super.Motor {
      override val delegate = new Impl(userCtx,out,indent)      
    }
    def apply(pr: utils.ParamReader, userCtx:UserCtx) = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
    def apply(out:Writer, indent:Int=0, userCtx:UserCtx) = new Motor(out,indent,userCtx)
  }
  object cre extends loader.core.Core.Abstract with DefImpl {
    class Motor(val out:Writer, val indent:Int=0, val userCtx:UserCtx) extends super.Motor {
      override val delegate = new Impl(userCtx,out,indent)      
    }
    def apply(pr: utils.ParamReader, userCtx:UserCtx) = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
    def apply(out:Writer, indent:Int=0, userCtx:UserCtx) = new Motor(out,indent,userCtx)
  }
  
}

//XXX remove or adapt: clash of names, Motor contravariant
abstract class Proc {
  import loader.core._
  abstract class Motor[P<:definition.Processor](val userCtx:UserContext[P])
  val ctx:CtxCore
  val ext:ExtCore
  val cre:Core
}

