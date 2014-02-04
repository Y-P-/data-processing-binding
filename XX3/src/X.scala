import loader._
import loader.context.ClassContext
import loader.core.definition.Status
import loader.features.DefaultCtxEventsCbk
import loader.core.callbacks.CallbacksBuilder
import loader.features.DefaultAuditHandler
import loader.audit.AuditRecorder
import loader.features.StandardAuditLogger
import loader.audit.IdScheme
import java.io.OutputStream
import utils.RegexReplacer
import java.util.regex.Pattern
import loader.core.names.QName


object Test {
  
  def main(args:Array[String]):Unit = {
    val userCtx = new loader.core.UserContext[Element { type Kind<:String }] {
      override def eventHandler = new DefaultAuditHandler(new StandardAuditLogger(IdScheme.ctx,5),new AuditRecorder(5))
      override def isInclude(elt:E)(s:elt.Kind):Boolean = !elt.isInstanceOf[loader.core.CtxCore.Def#Terminal]
      override def solveInclude(elt:E)(s:elt.Kind) = new loader.parsers.Struct(256,40,false).run(new java.io.File("verysmall1.txt").toURI, "UTF-8")
      override def qName(elt:E) = {
        val q=super.qName(elt)
        if (q!=null) new QName(elt.name.capitalize,q.prefix,q.isAttrib)
        else         new QName(elt.name.capitalize)
      }
    }
    val p = new parsers.Struct(256,40,false)
    
    if (true) {
      val out = new java.io.PrintWriter(System.out)
      val m = new motors.Struct.ctx.Motor(out,2,userCtx)
      val r = p.run(new java.io.File("small").toURI, "UTF-8")(m(ClassContext(classOf[test.Data.Top])))
      out.flush
    } else {
      val out = new java.io.PrintWriter(utils.NullStream)
      val m = new motors.Struct.ctx.Motor(out,2,userCtx)
      val r = p.run(new java.io.File("small").toURI, "UTF-8")(m(ClassContext(classOf[test.Data.Top]), new DefaultCtxEventsCbk[Unit,String]))      
      out.flush
    }
  } 
}
