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
  sealed class Data protected (private[this] val b:Binder[DefImpl#EltBase]#I) {
    final def subData(d:EClass, e:DefImpl#EltBase) = Data(d,e,b)   //build a new data on this binder ; useful only when dealing with lists
    final def set(x:AnyRef,e:DefImpl#EltBase)      = b.set(x,e)    //show the set method
    def close(e:DefImpl#EltBase)                   = if (e.eltCtx.update) { //this assigns the data to the bound object (lists)
      val old = b.read()
      val t1  = b.asT
      val t2  = {b.close(e); b.asT}
      val r = e.eltCtx.merge(t1,t2)
      if      (r eq t2) {}                 //t2 is already inside the field: do nothing
      else if (r eq t1) b.set(old, e)    //reset old value
      else {             
        for (x<-r) b.rcv(x,e)            //rebuild collection using underlying Builder ; bypass conversions (already done!)
        b.close(e)                       //set the result
      }
    } else b.close(e)    
  }
  private class StcData (val on:AnyRef,b:Binder[DefImpl#EltBase]#I,private[this] var seqs:Map[String,Binder[DefImpl#EltBase]#I]) extends Data(b) {
    //we use a variable to store an immutable map. We could have used a mutable map, but we expect few sequences per element and small immutable maps have a low memory footprint and are faster
    final def getOrElseUpdate(name:String, b: =>Binder[DefImpl#EltBase]#I) = seqs.get(name) match {
      case Some(x) => x
      case None    => val x=b; seqs = seqs + ((name,x)); x
    }
    //close all current sequences and assign structure to its owner
    override final def close(e:DefImpl#EltBase) = {
      for (x <- seqs) x._2.close(e)
      if (e.parent!=null) set(on,e)
    }
  }
  
  /** possible status for broken data */
  object EltClass extends Enumeration {
    type EltClass = Value
    val struct  = Value
    val term    = Value
    val list    = Value
  }  
  
  /** returns the kind of data managed by an Elt */
  protected trait EClass {
    def eClass(e:DefImpl#EltBase):EltClass.Value
  }
  
  /** factory for Data */
  protected object Data {
    protected def getObject(e:DefImpl#EltBase, i:Binder[DefImpl#EltBase]#I):AnyRef = {
      val x = e.eltCtx
      if (x.update) {
        val o = i.read.asInstanceOf[AnyRef]
        if (o!=null) return o
      }
      x.spawn(i)
    }
    def apply(d:EClass, e:DefImpl#EltBase, i:Binder[DefImpl#EltBase]#I):Data = d.eClass(e) match {
      case EltClass.term     => new Data(i)
      case EltClass.struct   => new StcData(getObject(e,i),i,Map.empty)
      case EltClass.list     => new Data(i.subInstance)  //let us enter the sub layer for the binder
    }
  }
  
  trait DefImpl extends super.DefImpl {
    type Value      = String
    type Key        = String
    type Ret        = Unit
    //override type Data = ObjectMotor.Data
    type BaseParser = ParserBuilder //any parser
    type UCtx[-p<:BaseParser]>:Null<:ObjectMotor.UCtx[p,this.type]
    final def baseParserClass = classOf[BaseParser]
    
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
      def eClass(e:DefImpl#EltBase):EltClass.Value      
      final def binder(e:Elt,on:AnyRef) = Binder(DataActor(on.getClass,e.name,"bsfm").get, e.eltCtx.converters, acd(e), isSeq(e) || depth(e)>0)(on)
      //building the data: we must first examine the kind of parent we have
      def getData(e:Elt):Data = e.parent match {
        case null                            => new StcData(on,null,Map.empty)                            //this is the top object we are filling/building
        case s if eClass(s)==EltClass.struct => val d = data(s).asInstanceOf[StcData]                     //parent is an object: we must bind one of its field
                                                Data(this,e,
                                                  if (isSeq(e)) d.getOrElseUpdate(e.name, binder(e,d.on).subInstance)   //for a sequence, find or create the appropriate binder
                                                  else          binder(e,d.on)                                          //otherwise, simply bind the field
                                                )
        case l if eClass(l)==EltClass.list   => data(l).subData(this,e)                                   //use the binder defined for the list
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
    type UCtx[-p<:BaseParser] = ObjectMotor.UCtx[p,this.type] with CtxCore.UsrCtx[p,this.type]
    final def baseUCtxClass   = null //XXX classOf[UCtx[_]]
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
      final def eClass(e:DefImpl#EltBase):EltClass.Value = e match {
        case _:Terminal => EltClass.term
        case _:Struct   => EltClass.struct
        case _:List     => EltClass.list
      }
    }
    def apply(pr: utils.ParamReader):Dlg   = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }
  
  /** Actual implementation for EtxCore.
   *  As we have no context, we will have to infer the properties of each element.
   *  This processor can spawn any class.
   */
  //TODO
  object ext extends loader.core.ExtCore.Abstract[Data] with DefImpl {
    type UCtx[-p<:BaseParser] = ObjectMotor.UCtx[p,this.type]
    final def baseUCtxClass   = classOf[UCtx[_]]
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
      final def eClass(e:DefImpl#EltBase):EltClass.Value = EltClass.struct
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
      /** if true, a current field (object) will be updated rather than created from scratch */
      def update:Boolean = false
      /** Converters to use */
      def converters = StandardSolver()
      /** Spawning new elements ; note that this can be overridden to create inner objects if necessary */
      def spawn(i:Binder[DefImpl#EltBase]#I):AnyRef = i.eltClass.asInstanceOf[Class[_<:AnyRef]].newInstance
      /** Merging collections; called only if update is true.
       *  If you don't return either 'cur' or 'read' or a simple operation such as cur ++ read, and the
       *  collection is more than one level deep, you're probably asking for problems.
       *  You had better know exactly what you are doing.
       *  By default, we concatenate the old and the new values.
       *  @returns 'cur' if you keep the old (not null) value, 'read' if you keep the new, or a new Traversable for the merge
       */
      def merge(cur:Traversable[Any],read:Traversable[Any]):Traversable[Any] = cur ++ read
    }
  }
}
