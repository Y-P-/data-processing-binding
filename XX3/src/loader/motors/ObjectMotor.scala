package loader.motors

import java.io.{Writer,FileWriter,File,OutputStreamWriter}
import loader.core._
import loader.core.definition._
import loader.core.ParserBuilder
import loader.reflect._
import loader.core.context.Context

/** This processor is used to spawn new objects.
 *  A.k.a Data Binding.
 *  
 *  The processor itself is rather simple, but it relies heavily on the loader.reflect package
 *  which contains the basic API for filling an object field dynamically.
 */
object ObjectMotor extends ProcessorImpl {

  /** The Data component for building objects is a bit more convoluted than for most other processors.
   *  It has to keep track of things such as the current object we work on, the binder to upper layer object, and for structures, known sequences
   */
  sealed class Data protected (private[this] val b:Binder[CtxCore#Elt]#I) {
    final def subData(e:CtxCore#Elt)      = Data(e,b)   //build a new data on this binder ; useful only when dealing with lists
    final def set(x:AnyRef,e:CtxCore#Elt) = b.set(x,e)  //show the set method
    def close(e:CtxCore#Elt)              = b.close(e)  //show the close method : this assigns the data to the bound object (lists)
  }
  private class StcData (val on:AnyRef,b:Binder[CtxCore#Elt]#I,private[this] var seqs:Map[String,Binder[CtxCore#Elt]#I]) extends Data(b) {
    //we use a variable to store an immutable map. We could have used a mutable map, but we expect few sequences per element and small immutable maps have a low memory footprint and are faster
    final def getOrElseUpdate(name:String, b: =>Binder[CtxCore#Elt]#I) = seqs.get(name) match {
      case Some(x) => x
      case None    => val x=b; seqs = seqs + ((name,x)); x
    }
    //close all current sequences and assign structure to its owner
    override final def close(e:CtxCore#Elt) = {
      for (x <- seqs) x._2.close(e)
      if (e.parent!=null) set(on,e)
    }
  }
  
  /** factory for Data */
  protected object Data {
    def apply(e:CtxCore#Elt,i:Binder[CtxCore#EltBase]#I):Data = e match {
      case _:CtxCore#Terminal => new Data(i)
      case _:CtxCore#Struct   => new StcData(i.eltClass.asInstanceOf[Class[_<:AnyRef]].newInstance,i,Map.empty)
      case _:CtxCore#List     => new Data(i.subInstance)  //let us enter the sub layer for the binder
    }
  }
  
  implicit final class ToACD(val fd:Context#FieldMapping) extends AutoConvertData {
    final def convert: String = fd.annot.convert
    final def check: String   = fd.annot.check
    final def param: String   = fd.annot.param
    final def valid: String   = fd.annot.valid
  }
  val dummy = new AutoConvertData {
    def convert: String = ""
    def check: String = ""
    def param: String = ""
    def valid: String = ""
  }  
  trait DefImpl extends super.DefImpl with CtxCore {
    type Value      = String
    type Key        = String
    type Ret        = Unit
    override type Data = ObjectMotor.Data
    type BaseParser = ParserBuilder //any parser
    type UCtx[-p<:BaseParser] = UsrCtx[p,this.type]
    final def baseParserClass = classOf[BaseParser]
    final def baseUCtxClass   = classOf[UCtx[_]]
    
    val noKey = ""
  
    /**
     * @param on, the object to fill up
     */    
    abstract class DlgBase(on:AnyRef) extends super.DlgBase {this:Dlg=>
      type Result = AnyRef
      final def binder(e:Elt,on:AnyRef) = Binder(DataActor(on.getClass,e.name,"bsfm").get, StandardSolver(), e.fd, e.fd.isSeq || e.fd.depth>0)(on)
      //building the data: we must first examine the kind of parent we have
      def getData(e:Elt):Data = e.parent match {
        case null     => new StcData(on,null,Map.empty)                                          //this is the top object we are filling/building
        case s:Struct => val d = s.data.asInstanceOf[StcData]                                    //parent is an object: we must bind one of its field
                         Data(e,
                           if (e.fd.isSeq) d.getOrElseUpdate(e.name, binder(e,d.on).subInstance) //for a sequence, find or create the appropriate binder
                           else            binder(e,d.on)                                        //otherwise, simply bind the field
                         )
        case l:List   => l.data.subData(e)                                                       //use the binder defined for the list
      }
  
      //pretty simple processor here: actual complexity is in Data
      def onInit(e:Elt):Unit           = {}
      def onBeg(e:Elt): Unit           = {}
      def onVal(e:Elt,v:String): Unit  = e.data.set(v,e)  //set terminal field
      def onEnd(e:Elt): Unit           = e.data.close(e)  //'close' container (list/struct) and assign result to upper layer
      def onChild(e:Elt,child: Elt, r: Unit): Unit = {}

      def onInit():Unit = {}
      def onExit():AnyRef = on
    }
  }
  
  protected def readParams(pr: utils.ParamReader) = ???
  
  /** Actual implementation. Due to the nature of this processor, we have only the ctx implementation
   */
  object ctx extends loader.core.CtxCore.Abstract[Data] with DefImpl {
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase
    def apply(pr: utils.ParamReader):Dlg   = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }
  val ext = null
  val cre = null
}
