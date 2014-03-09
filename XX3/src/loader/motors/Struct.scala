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

object Struct extends ProcessorImpl {
  
  trait DefImpl extends super.DefImpl {
    type Value      = String
    type Key        = String
    type Ret        = Int
    type BaseParser = ParserBuilder { type BaseProcessor>:DefImpl.this.type } //any parser
    
    def getData[P<:BaseParser](parent:Element[P],s:Status):Null = null
    val noKey = ""
  
    /**
     * @param out, where to write to
     * @param indent, indent value as space number ; 0 means all output on one line
     */    
    abstract class Impl[P<:BaseParser](val out:Writer, val indent:Int) extends super.Impl[P] {
      type Result = Unit
    
      private def newLine(e:Elt):Unit   = out.write(if (indent>0) Indent((e.depth-1)*indent) else " ")
      private def quote(s:String):Unit  = out.write(if(parser.px.Data.isChar(s)) s else s""""$s"""")
      private def tag(e:Elt):Unit       = { newLine(e); if (!e.name.isEmpty) { quote(e.name); out.write(s" = ") } }
  
      def onInit(e:Elt):Unit          = {}
      def onBeg(e:Elt): Unit          = if (e.parent!=null) { tag(e); out.write("{") }
      def onVal(e:Elt,v: String): Int = { tag(e); quote(v); out.flush; 1 }
      def onEnd(e:Elt): Int           = if (e.parent!=null) { newLine(e); out.write("}"); out.flush; 1 } else 0
      def onChild(e:Elt,r: Int): Unit = {}

      def onInit():Unit = {}
      def onExit():Unit = out.flush
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
  
  /** Actual implementations. Due to the nature of this processor, we have all three implementations available.
   *  Implementation 'ext' doesn't bring anything more than 'cre', except it's a bit slower!
   */
  object ctx extends loader.core.CtxCore.Abstract[Null] with DefImpl {
    protected[this] class Launcher[-P<:BaseParser](out:Writer,indent:Int=0) extends Impl[P](out,indent) with super.Launcher[P] {
      def this(p:(Writer,Int)) = this(p._1,p._2)
    }
    def apply[P<:BaseParser](pr: utils.ParamReader):Motor[P]  = new Launcher(readParams(pr))
    def apply[P<:BaseParser](out:Writer,indent:Int=0):Motor[P] = new Launcher(out,indent)
  }
  object ext extends loader.core.ExtCore.Abstract[Null] with DefImpl {
    protected[this] class Launcher[-P<:BaseParser](out:Writer,indent:Int=0) extends Impl[P](out,indent) with super.Launcher[P] {
      def this(p:(Writer,Int)) = this(p._1,p._2)
    }
    def apply[P<:BaseParser](pr: utils.ParamReader):Motor[P] = new Launcher(readParams(pr))
    def apply[P<:BaseParser](out:Writer, indent:Int=0)    = new Launcher(out,indent)
  }
  object cre extends loader.core.Core.Abstract with DefImpl {
    protected[this] class Launcher[-P<:BaseParser](out:Writer,indent:Int=0) extends Impl[P](out,indent) with super.Launcher[P] {
      def this(p:(Writer,Int)) = this(p._1,p._2)
    }
    def apply[P<:BaseParser](pr: utils.ParamReader):Motor[P]    = new Launcher(readParams(pr))
    def apply[P<:BaseParser](out:Writer, indent:Int=0):Motor[P] = new Launcher(out,indent)
  }
  
}
