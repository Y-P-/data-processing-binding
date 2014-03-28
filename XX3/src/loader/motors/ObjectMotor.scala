package loader.motors

import java.io.{Writer,FileWriter,File,OutputStreamWriter}
import utils.reflect._
import loader.core._
import loader.core.definition._
import loader.core.ParserBuilder
import loader.core.context.Context

/** This processor is used to spawn new objects.
 *  A.k.a Data Binding.
 *  
 *  The processor itself is rather simple, but it relies heavily on the loader.reflect package
 *  which contains the basic API for filling an object field dynamically.
 */
object ObjectMotor extends ProcessorImpl {

  /** The Data component for building objects is a bit more convoluted than for most other processors.
   *  It has to keep track of things such as the current object we work on, the binder to upper layer object, and for structures, known sequences.
   */
  sealed class Data protected (private[this] val b:Binder#I) { //default implementation, used for Terminal
    final def set(x:AnyRef)       = b.set(x)                   //show the set method
    def close(e:DefImpl#EltBase)  = ()                         //no close for element
    def apply(kind:Int,d:DefImpl#DlgBase)(e:d.EX):Data = ???   //data factory for a sub-object, obviously an error for Terminal
  }
  private class ListData (b:Binder#I) extends Data(b) {
    override def close(e:DefImpl#EltBase) = if (e.eltCtx.update) { //this assigns the data to the bound object (lists)
      val old = b.read()
      val t1  = b.asT
      val t2  = {b.close(); b.asT}
      val r = e.eltCtx.merge(t1,t2)
      if      (r eq t2) {}          //t2 is already inside the field: do nothing
      else if (r eq t1) b.set(old)  //reset old value
      else {             
        for (x<-r) b.rcv(x)         //rebuild collection using underlying Builder ; bypass conversions (already done!)
        b.close()                   //set the result
      }
    } else b.close()    
    override def apply(kind:Int,d:DefImpl#DlgBase)(e:d.EX):Data = Data(kind,e,b)
  }
  private class StcData (val on:AnyRef,b:Binder#I,private[this] var seqs:Map[String,Binder#I]) extends Data(b) {
    protected final def localClose() = for (x <- seqs) x._2.close()    //close all current sequences
    override def close(e:DefImpl#EltBase) = { localClose(); set(on) }  //and assign structure to its owner
    override def apply(kind:Int,d:DefImpl#DlgBase)(e:d.EX):Data = {
      val b = d.binder(e,on)
      Data(kind,e,  //seqs are kept in an immutable map (we expect it to stay small) stored in a local var which we update when needed 
        if (d.isSeq(e)) seqs.getOrElse(e.name,{val x=b.subInstance; seqs=seqs+((e.name,x)); x})
        else            b //otherwise, simply bind the field
      )      
    }
  }
  private class RootData (on:AnyRef) extends StcData(on,null,Map.empty) {
    override final def close(e:DefImpl#EltBase) = localClose()  //no owner to assign to
  }    
  
  /** factory for Data */
  protected object Data {
    //gets the AnyRef associated with a field of a structure e through the binder i
    private def getObject(e:DefImpl#EltBase, i:Binder#I):AnyRef = {
      val x = e.eltCtx
      if (x.update) {
        val o = i.read.asInstanceOf[AnyRef]
        if (o!=null) return o
      }
      x.spawn(i)
    }
    def apply(kind:Int, e:DefImpl#EltBase, i:Binder#I):Data = kind match {
      case CtxCore.term   => new Data(i)
      case CtxCore.struct => new StcData(getObject(e,i),i,Map.empty)
      case CtxCore.list   => new ListData(i.subInstance)  //let us enter the sub layer for the binder
    }
  }
  
  
  /** We make a common implementation for ctx and ext, even though it is likely marginally less efficient
   *  It is a bit more complex too, but this makes only one algorithm to maintain.
   */
  trait DefImpl extends super.DefImpl {
    type Value      = String
    type Key        = String
    type Ret        = Unit
    type BaseParser = ParserBuilder //any parser
    type UCtx[-p<:BaseParser]>:Null<:ObjectMotor.UCtx[p,this.type]
    final def baseParserClass = classOf[BaseParser]
    
    val noKey = ""
  
    /**
     * @param on, the object to fill up
     */    
    abstract class DlgBase(on:AnyRef) extends super.DlgBase {this:Dlg=>
      type Result = AnyRef
      type EX = Elt
      //these abstract methods replace what we could get directly from CtxCore ; they will have to be filled up in ext
      def isSeq(e:Elt):Boolean
      def isSeq(parent:Elt,s:Status):Boolean
      def depth(parent:Elt,s:Status):Int
      def acd(parent:Elt,s:Status):AutoConvertData
      def data(e:Elt):Data
      def eClass(parent:Elt,s:Status):Int
      protected[this] var kind:Int = -1           //perf cache to avoid two expensive calls
      protected[this] var actor:DataActor = null  //perf cache to avoid two expensive calls
      //note: the binder for e is established in regard to its parent. Hence we use the parents converters!
      final def binder(e:Elt,on:AnyRef) = try {
        if (actor==null) dactor(e.name,on)
        Binder(actor, e.parent.eltCtx.converters, acd(e.parent,e.status), isSeq(e.parent,e.status) || depth(e.parent,e.status)>0)(on)
      } finally {
        actor=null
      }
      final protected def dactor(name:String,on:AnyRef):Unit = actor = DataActor(on.getClass,name,"bsfm").get
      //building the data: we must first examine the kind of parent we have
      def getData(e:Elt):Data = (try { data(e.parent) } catch {
        case _:NullPointerException => return new RootData(on)  //not beautiful, but efficient : don't test for each element, but catch exception for the top element that doesn't follow the same logic
      })(kind,this)(e)
  
      //pretty simple processor here: actual complexity is in Data
      def onInit(e:Elt):Unit           = {}
      def onBeg(e:Elt): Unit           = {}
      def onVal(e:Elt,v:String): Unit  = data(e).set(v)    //set terminal field
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
    override type UCtx[-p<:BaseParser] = ObjectMotor.UCtx[p,this.type] with CtxCore.UsrCtx[p,this.type]
    final def baseUCtxClass   = null //XXX classOf[UCtx[_]] => use TypeTags for dynamic class checking
    implicit final class ToACD(val fd:Context#FieldMapping) extends AutoConvertData {
      final def convert: String = fd.annot.convert
      final def check: String   = fd.annot.check
      final def param: String   = fd.annot.param
      final def valid: String   = fd.annot.valid
    }
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase {
      final def data(e:Elt):Data                         = e.data
      final def isSeq(e:Elt):Boolean                     = e.fd.isSeq
      final def isSeq(parent:Elt,s:Status):Boolean       = s.fd.isSeq
      final def depth(parent:Elt,s:Status):Int           = s.fd.depth
      final def acd(parent:Elt,s:Status):AutoConvertData = s.fd
      override def eClass(parent:Elt,s:Status):Int = {
        kind=super.eClass(parent,s)
        //we don't believe the upper layer for terminals ; possibly we have to guess:
        //if the Terminal has no String -> X converter, we'd be screwed!
        //in that case we assume that the user wants us to guess by looking at X and using it as the next layer
        //this only works if the structure ids are indeed class names!
        if (parent!=null && kind==CtxCore.term) parent.data match {
          case d:StcData => dactor(s.key,d.on)
                            val ec:ObjectMotor.UCtx[parent.Builder,ctx.type]#EltCtx = parent.eltCtx
                            val b = Binder(actor, ec.converters, acd(parent,s), isSeq(parent,s) || depth(parent,s)>0)(d.on)
                            println(b.endClass)
                            //if (ec.converters.stringSolver(b.eltClass)==None) kind=CtxCore.struct 
          case _ =>
        }
        kind
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
  /*
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
      final def eClass(e:DefImpl#EltBase):Int = CtxCore.struct
    }
    def apply(pr: utils.ParamReader):Dlg   = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }
  */
  def ext = ???
  /** This cannot be implemented: we have to maintain a context. */
  def cre = ???
  
  /** We define a specific UserType here.
   *  This processor indeed requires specific information to work, and these can be tuned by the end user.
   */
  trait UCtx[-P<:ParserBuilder,-M<:DefImpl] extends UsrCtx[P,M] {
    type EltCtx>:Null<:EltCtxBase
    protected[this] trait EltCtxBase extends super.EltCtxBase { this:EltCtx=>
      /** if true, a current field (object) will be updated rather than created from scratch */
      def update:Boolean = false
      /** Converters to use */
      def converters:ConversionSolver = new ConversionSolver()
      /** Spawning new elements ; note that this can be overridden to create inner objects if necessary */
      def spawn(i:Binder#I):AnyRef = i.eltClass.asInstanceOf[Class[_<:AnyRef]].newInstance
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
