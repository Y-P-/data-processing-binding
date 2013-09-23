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
  /** 
   * @param tabs the current tabulation value
   * @param written is true if the starting tag has been written
   */
  class Data(val tabs:Int,var written:Boolean) {
    def this(tabs:Int) = this(tabs,false)
  }
  
  trait DefImpl extends loader.core.ExtCore.Def {
    type Kind    = String
    type Ret     = Unit
    type Data    = Struct.Data
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
      //also note that we must act on the first such call, otherwise the call will write data before we write the container tag!
      //this means that we cannot act with onChild (too late: sub data has been written already) ; this only leaves the onBeg method
      //of any child to write the starting tag of its parent, while the onVal can account for a valued element.
      //This has the nasty side effect that unlike what is expected, onBeg is not acting on the current element, but its parent! 
      //This is not expected by the framework and we must correct this misbeheavior here too ; we do this by testing e.parent!=null
      //to avoid unwanted output on the top element.
      protected def getData(parent:Element,s:Status):Data = new Data((if (parent!=null) parent.data.tabs+indent else -indent))

      private def newLine(e:Element):Unit   = out.write(if (indent>0) Indent(e.data.tabs) else " ")
      private def tag(e:Element):Unit       = { newLine(e); if (!e.name.isEmpty) { quote(e.outName); out.write(s" = ") } }
      private def quote(s:String):Unit      = out.write(if(parser.px.Data.isChar(s)) s else s""""$s"""")
      private def writeBeg(e:Element):Unit  = if (e!=null && e.parent!=null && !e.data.written) { tag(e); out.write("{"); e.data.written=true }
  
      protected def onBeg(e:Element): Unit           = writeBeg(e.parent)
      protected def onVal(e:Element,v: String): Ret  = { tag(e); quote(v); out.flush }
      protected def onEnd(e:Element): Ret            = if (e.parent!=null) { newLine(e); out.write("}"); out.flush }
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
  abstract class Motor[E<:definition.Def#Elt](val userCtx:UserContext[E])
  val ctx:CtxCore.Impl
  val ext:ExtCore.Impl
}

