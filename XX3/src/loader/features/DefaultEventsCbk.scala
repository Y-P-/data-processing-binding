package loader.features

import loader.core.callbacks.Callback
import loader.core.callbacks.CallbacksBuilder
import loader.core.CtxCore._
import loader.core.ParserBuilder
import loader.core.events.Event

/** Provides a base class where all idx are well identified for a common switch */
trait DefaultEvent extends Event

/** Events defined for DefaultCtxEventsCbk
 */
case class ReadTagEvt[K>:Null](v:Any,s:K)                            extends DefaultEvent { val idx=DefaultCtxEventsCbk.readTagIdx }
case class FastWarnEvt()                                             extends DefaultEvent { val idx=DefaultCtxEventsCbk.fastWarnIdx }
case class FastDisabledEvt()                                         extends DefaultEvent { val idx=DefaultCtxEventsCbk.fastDisabledIdx }
case class DefaultValueEvt(name:String)                              extends DefaultEvent { val idx=DefaultCtxEventsCbk.defaultValueIdx }
case class InvalidCardinalityEvt(name:String,min:Int,max:Int,nb:Int) extends DefaultEvent { val idx=DefaultCtxEventsCbk.invalidCardinalityIdx }
case class OutOfSeqEvt(name:String)                                  extends DefaultEvent { val idx=DefaultCtxEventsCbk.outOfSeqIdx }
case class IllegalRepetitionEvt(name:String)                         extends DefaultEvent { val idx=DefaultCtxEventsCbk.illegalRepetitionIdx }
case class TagNotFoundEvt(name:String,min:Int)                       extends DefaultEvent { val idx=DefaultCtxEventsCbk.tagNotFoundIdx }
case class IgnoredTagEvt(name:String)                                extends DefaultEvent { val idx=DefaultCtxEventsCbk.ignoredTagIdx }
case class IncludeSuccessEvt[K>:Null](info:K)                        extends DefaultEvent { val idx=DefaultCtxEventsCbk.includeSuccessIdx }


/** A callback that does some internal analysis in order to send some events about
 *  the processing state. In particular, it uses the Context#FieldMapping definition
 *  in order to check the validity of read fields.
 */
class DefaultCtxEventsCbk[R0,K>:Null] extends Callback[Def#Elt,Status,R0,K] {
  /** Note that to avoid useless calls, we break down the Default event generator into three pieces,
   *  one for each kind of element. There is scarce common code between them.
   */
  override def apply(e0:Def#Elt):Inner = e0 match {
    //Struct events
    case stc: Def#Struct => new Inner(e0) {
      override def onName[S<:Status](name:String, f: (String)=>S):S = {
        import ParserBuilder._
        val s = try { f(name) } catch {
          case x:SkipException => elt(IgnoredTagEvt(name)); throw x
        }
        if (s.broken) if (elt.fd.isSeq) elt(OutOfSeqEvt(name)) else elt(IllegalRepetitionEvt(name))
        s
      }
      override def onBeg(f: => Unit):Unit = {
        if (stc.canFast)
          if (stc.doFast) stc(FastWarnEvt()) else stc(FastDisabledEvt())
        f
      }
      override def onVal[R<:R0](s:K, f: (K)=>R):R = {
        val r = f(s)
        elt(IncludeSuccessEvt(s))
        elt(ReadTagEvt(r, s))
        r
      }
      override def onEnd[R<:R0](f: =>R):R = {
        if (stc.fd.loader!=null)
          for (f <- stc.tags.values)
            if (!stc.seen.contains(f.inName))
              if (f.annot.min==0) stc(DefaultValueEvt(f.inName))
              else                stc(TagNotFoundEvt(f.inName,f.annot.min))
            else if (f.isSeq) {
              val x = stc.seen(f.inName)
              if (f.annot.min>0 && x<f.annot.min || f.annot.max>0 && x>f.annot.max)
                stc(InvalidCardinalityEvt(f.inName, f.annot.min, f.annot.max, x))
            }
        val r = f
        elt(ReadTagEvt(r, null))
        r
      }
    }
    //Lists events
    case lst: Def#List => new Inner(e0) {
      override def onVal[R<:R0](s:K, f: (K)=>R):R = {
        val r = f(s)
        elt(ReadTagEvt(r, s))
        r
      }
      override def onEnd[R<:R0](f: =>R):R = {
        if (lst.fd.annot.min>0 && lst.index<lst.fd.annot.min || lst.fd.annot.max>0 && lst.index>lst.fd.annot.max)
          lst.parent(InvalidCardinalityEvt(lst.name, lst.fd.annot.min, lst.fd.annot.max, lst.index))
        val r = f
        elt(ReadTagEvt(r, null))
        r
      }
    }
    //Terminal events
    case _ => new Inner(e0) {
      override def onVal[R<:R0](s:K, f: (K)=>R):R = {
        val r = f(s)
        elt(ReadTagEvt(r, s))
        r
      }
    }
  }
}
object DefaultCtxEventsCbk {
  import scala.language.implicitConversions
  //a builder for Callbacks
  def builder[R,K>:Null] = new CallbacksBuilder[loader.motors.Struct.ctx.Elt,loader.core.CtxCore.Status,R,K]
  //converting DefaultCtxEventsCbk into a Callbacks recursive tree
  implicit def cbks[R,K>:Null](cbk:DefaultCtxEventsCbk[R,K]) = builder(cbk)
  
  val readTagIdx = 0
  val fastWarnIdx = 1
  val fastDisabledIdx = 2
  val defaultValueIdx = 3
  val invalidCardinalityIdx = 4
  val outOfSeqIdx = 5
  val illegalRepetitionIdx = 6
  val tagNotFoundIdx = 7
  val ignoredTagIdx = 8
  val includeSuccessIdx = 9
}