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
  sealed class Data protected (private[this] val b:Binder[Processor#EltBase]#I) {
    final def subData(d:EClass, e:Processor#EltBase) = Data(d,e,b)   //build a new data on this binder ; useful only when dealing with lists
    final def set(x:AnyRef,e:Processor#EltBase)      = b.set(x,e)  //show the set method
    def close(e:Processor#EltBase)                   = b.close(e)  //show the close method : this assigns the data to the bound object (lists)
  }
  private class StcData (val on:AnyRef,b:Binder[Processor#EltBase]#I,private[this] var seqs:Map[String,Binder[Processor#EltBase]#I]) extends Data(b) {
    //we use a variable to store an immutable map. We could have used a mutable map, but we expect few sequences per element and small immutable maps have a low memory footprint and are faster
    final def getOrElseUpdate(name:String, b: =>Binder[Processor#EltBase]#I) = seqs.get(name) match {
      case Some(x) => x
      case None    => val x=b; seqs = seqs + ((name,x)); x
    }
    //close all current sequences and assign structure to its owner
    override final def close(e:Processor#EltBase) = {
      for (x <- seqs) x._2.close(e)
      if (e.parent!=null) set(on,e)
    }
  }
  
  protected val struct   = 1
  protected val term     = 0
  protected val list     = 2
  
  /** returns the kind of data managed by an Elt */
  protected trait EClass {
    def eClass(e:Processor#EltBase):Int
  }
  
  /** factory for Data */
  protected object Data {
    def apply(d:EClass, e:Processor#EltBase, i:Binder[Processor#EltBase]#I):Data = d.eClass(e) match {
      case `term`     => new Data(i)
      case `struct`   => new StcData(i.eltClass.asInstanceOf[Class[_<:AnyRef]].newInstance,i,Map.empty)
      case `list`     => new Data(i.subInstance)  //let us enter the sub layer for the binder
    }
  }
  
  trait DefImpl extends super.DefImpl {
    type Value      = String
    type Key        = String
    type Ret        = Unit
    //override type Data = ObjectMotor.Data
    type BaseParser = ParserBuilder //any parser
    type UCtx[-p<:BaseParser] = ObjectMotor.UCtx[p,this.type]
    final def baseParserClass = classOf[BaseParser]
    final def baseUCtxClass   = classOf[UCtx[_]]
    
    val noKey = ""
  
    /**
     * @param on, the object to fill up
     */    
    abstract class DlgBase(on:AnyRef) extends super.DlgBase with EClass {this:Dlg=>
      type Result = AnyRef
      protected def isSeq(e:Elt):Boolean
      protected def depth(e:Elt):Int
      protected def acd(e:Elt):AutoConvertData
      protected def data(e:Elt):Data
      def eClass(e:Processor#EltBase):Int      
      final def binder(e:Elt,on:AnyRef) = Binder(DataActor(on.getClass,e.name,"bsfm").get, e.eltCtx.converters, acd(e), isSeq(e) || depth(e)>0)(on)
      //building the data: we must first examine the kind of parent we have
      def getData(e:Elt):Data = e.parent match {
        case null                   => new StcData(on,null,Map.empty)                            //this is the top object we are filling/building
        case s if eClass(s)==struct => val d = data(s).asInstanceOf[StcData]                     //parent is an object: we must bind one of its field
                         Data(this,e,
                           if (isSeq(e)) d.getOrElseUpdate(e.name, binder(e,d.on).subInstance)   //for a sequence, find or create the appropriate binder
                           else          binder(e,d.on)                                          //otherwise, simply bind the field
                         )
        case l if eClass(l)==list   => data(l).subData(this,e)                                   //use the binder defined for the list
      }
  
      //pretty simple processor here: actual complexity is in Data
      def onInit(e:Elt):Unit           = {}
      def onBeg(e:Elt): Unit           = {}
      def onVal(e:Elt,v:String): Unit  = data(e).set(v,e)  //set terminal field
      def onEnd(e:Elt): Unit           = data(e).close(e)  //'close' container (list/struct) and assign result to upper layer
      def onChild(e:Elt,child: Elt, r: Unit): Unit = {}

      def onInit():Unit = {}
      def onExit():AnyRef = on
    }
  }
  
  protected def readParams(pr: utils.ParamReader) = ???
  
  /** Actual implementation for CtxCore.
   */
  object ctx extends loader.core.CtxCore.Abstract[Data] with DefImpl {
    implicit final class ToACD(val fd:Context#FieldMapping) extends AutoConvertData {
      final def convert: String = fd.annot.convert
      final def check: String   = fd.annot.check
      final def param: String   = fd.annot.param
      final def valid: String   = fd.annot.valid
    }
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase {
      protected final def data(e:Elt):Data           = e.data
      protected final def isSeq(e:Elt):Boolean       = e.fd.isSeq
      protected final def depth(e:Elt):Int           = e.fd.depth
      protected final def acd(e:Elt):AutoConvertData = e.fd
      final def eClass(e:Processor#EltBase):Int = e match {
        case _:Terminal => term
        case _:Struct   => struct
        case _:List     => list
      }
    }
    def apply(pr: utils.ParamReader):Dlg   = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }
  
  /** Actual implementation for EtxCore.
   *  As we have no context, we will have to infer the properties of each element.
   *  This processor can spawn any class.
   */
  object ext extends loader.core.ExtCore.Abstract[Data] with DefImpl {
    val dummy = new AutoConvertData {  //no data for conversions
      def convert: String = ""
      def check: String = ""
      def param: String = ""
      def valid: String = ""
    }
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase {
      protected def data(e:Elt):Data           = e.data
      protected def acd(e:Elt):AutoConvertData = dummy
      //we don't manage sequences
      protected final def isSeq(e:Elt):Boolean = false
      //depth inferred from the returned type
      protected final def depth(e:Elt):Int     = 0
      //type inferred from the returned type
      final def eClass(e:Processor#EltBase):Int = struct
    }
    def apply(pr: utils.ParamReader):Dlg   = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }
  
  /** This cannot be implemented: we have to maintain a context. */
  def cre = ???
  
  /** We define a specific UserType here.
   *  This processor indeed requires specific information to work, and these can be tuned by the end user.
   */
  trait UCtx[-P<:ParserBuilder,-M<:DefImpl] extends UsrCtx[P,M] {
    type EltCtx <: EltCtxBase
    protected[this] trait EltCtxBase extends super.EltCtxBase { this:EltCtx=>
      /** Converters to use */
      def converters = StandardSolver()
      /** Solving dynamic mappings */
      def solveDynamic(fd:Context#FieldMapping):Context#FieldMapping = null
    }
  }
}
