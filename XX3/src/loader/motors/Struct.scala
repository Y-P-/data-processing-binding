package loader.motors

import java.io.Writer
import loader._
import utils.Indent
import loader.core._
import loader.core.callbacks._
import loader.reflect.Converter
import java.io.FileWriter
import java.io.File
import java.io.OutputStreamWriter
import loader.core.ParserBuilder


object Struct extends Processor {self=>
  
  trait DefImpl extends loader.core.ExtCore.Def {
    type Kind    = String
    type Ret     = Unit
    type Data    = Null
    type Parser  = ParserBuilder#Impl  //any parser
    def kindClass = classOf[Kind]
    def parsClass = classOf[Parser]
    
    /**
     * @param out, where to write to
     * @param indent, indent value as space number ; 0 means all output on one line
     */
    abstract class Inner(val out:Writer, val indent:Int=0, userCtx:UserCtx) extends self.Motor(userCtx)
    
    trait MotorImpl {
      val userCtx:UserCtx
      val out:Writer  //where to write to
      val indent:Int  //indent value as space number ; 0 means all output on one line
    
      type Result = Unit
 
      //Note: because we work on unknown data structures (no context in ExtCore), we can only know the kind of a tag (final/container)
      //after either its onVal or its onChild method has been called.
      // - onVal   => we have a valued element: it begins as     name =
      // - onChild => we have a container element: it begins as  name = {
      // - onEnd   => we have an empty element: it begins as     name = {
      protected def getData(parent:Element,s:Status):Data = null

      private def newLine(e:Element):Unit   = out.write(if (indent>0) Indent((e.depth-1)*indent) else " ")
      private def quote(s:String):Unit      = out.write(if(parser.px.Data.isChar(s)) s else s""""$s"""")
      private def tag(e:Element):Unit       = { newLine(e); if (!e.name.isEmpty) { quote(e.localName); out.write(s" = ") } }
  
      protected def onInit(e:Element):Unit          = {}
      protected def onBeg(e:Element): Unit          = if (e.parent!=null) { tag(e); out.write("{") }
      protected def onVal(e:Element,v: String): Ret = { tag(e); quote(v); out.flush }
      protected def onEnd(e:Element): Ret           = if (e.parent!=null) { newLine(e); out.write("}"); out.flush }
      protected def onChild(e:Element,child: Element, r: Ret): Unit = {}

      protected def onInit():Unit = {}
      protected def onExit():Unit = out.flush
    }
  }
  
  protected def readParams(pr: utils.ParamReader) = {
    val append = pr("append", false)(Converter.CvBoolean.read(null, _))
    val outFile:File = pr("out", null.asInstanceOf[File])(Converter.CvFile.read(null, _))
    val out = if (outFile!=null) new FileWriter(outFile,append) else new OutputStreamWriter(System.out)
    val indent = pr("indent", 2)(Converter.CvInt.read(null, _))
    (out,indent)
  }
  
  object ctx extends loader.core.CtxCore.Impl with DefImpl {
    class Motor(out:Writer, indent:Int=0, userCtx:UserCtx) extends Inner(out,indent,userCtx) with super.Motor with MotorImpl
    def apply(pr: utils.ParamReader, userCtx:UserCtx) = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
  }
  object ext extends loader.core.ExtCore.Impl with DefImpl {
    class Motor(out:Writer, indent:Int=0, userCtx:UserCtx) extends Inner(out,indent,userCtx) with super.Motor with MotorImpl
    def apply(pr: utils.ParamReader, userCtx:UserCtx) = {
      val p = readParams(pr)
      new Motor(p._1,p._2,userCtx)
    }
  }
  
}

abstract class Processor {
  import loader.core._
  abstract class Motor[E<:definition.Def#Element](val userCtx:UserContext[E])
  val ctx:CtxCore.Impl
  val ext:ExtCore.Impl
}

