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

  /*-----------------------------------------------------------------------*/
  /*       SECTION I: Define Data, with no attaches to Elt                 */
  /*-----------------------------------------------------------------------*/
  
  /** The Data component for building objects is a bit more convoluted than for most other processors.
   *  It has to keep track of things such as the current object we work on, the binder to upper layer object, and for structures, known sequences.
   */
  sealed class Data protected (private[this] val b:Binder#I) { //default implementation, used for Terminal
    final def set(x:AnyRef)   = b.set(x)                   //show the set method
    def close(eltCtx:EltCtx)  = ()                         //no close for element
    def apply(name:String, info:Info):Data = ???           //data factory for a sub-object, obviously an error for Terminal
  }
  private class ListData (b:Binder#I) extends Data(b) {
    override def close(eltCtx:EltCtx) = if (eltCtx.update) { //this assigns the data to the bound object (lists)
      val old = b.read()
      val t1  = b.asT
      val t2  = {b.close(); b.asT}
      val r = eltCtx.merge(t1,t2)
      if      (r eq t2) {}          //t2 is already inside the field: do nothing
      else if (r eq t1) b.set(old)  //reset old value
      else {             
        for (x<-r) b.rcv(x)         //rebuild collection using underlying Builder ; bypass conversions (already done!)
        b.close()                   //set the result
      }
    } else b.close()    
    override def apply(name:String, info:Info):Data = Data(info.eClass,info.eltCtx,b)
  }
  private class StcData (val on:AnyRef,b:Binder#I,private[this] var seqs:Map[String,Binder#I]) extends Data(b) {
    protected final def localClose() = for (x <- seqs) x._2.close()    //close all current sequences
    override def close(eltCtx:EltCtx) = { localClose(); set(on) }  //and assign structure to its owner
    override def apply(name:String, info:Info):Data = {
      val b = Binder(DataActor(on.getClass,name,"bsfm").get, info.eltCtx.converters, info.fd, info.isSeq || info.depth>0)(on)
      Data(info.eClass, info.eltCtx,  //seqs are kept in an immutable map (we expect it to stay small) stored in a local var which we update when needed 
        if (info.isSeq) seqs.getOrElse(name,{val x=b.subInstance; seqs=seqs+((name,x)); x})
        else            b //otherwise, simply bind the field
      )      
    }
  }
  private class RootData (on:AnyRef) extends StcData(on,null,Map.empty) {
    override final def close(eltCtx:EltCtx) = localClose()  //no owner to assign to
  }    
  
  /** factory for Data */
  protected object Data {
    //gets the AnyRef associated with a field of a structure e through the binder i
    private def getObject(eltCtx:EltCtx, i:Binder#I):AnyRef = {
      if (eltCtx.update) {
        val o = i.read.asInstanceOf[AnyRef]
        if (o!=null) return o
      }
      eltCtx.spawn(i)
    }
    /** factory: this is the exclusive way to build a Data.
     *  @param kind,   the kind of Data we need
     *  @param eltCtx, the (parent) context for determining wether we update, and how we build a new object if needed
     *  @param i,      the binder instance attached to the element
     */
    def apply(kind:Int, eltCtx:EltCtx, i:Binder#I):Data = kind match {
      case CtxCore.term   => new Data(i)
      case CtxCore.struct => new StcData(getObject(eltCtx,i),i,Map.empty)
      case CtxCore.list   => new ListData(i.subInstance)  //let us enter the sub layer for the binder
    }
  }
  
  /** This is the interface requirement to build a Data.
   */
  trait Info {
    def eltCtx:EltCtx               //element context of element we are building the data for
    def fd:AutoConvertData          //data for auto converting the element we are building
    def isSeq:Boolean               //is the element being built part of a sequence ?
    def depth:Int                   //if element being built is a list, depth
    def eClass:Int                  //stc class for the element
   //XXX def converters:ConversionSolver //conversion solvers for the element
  }
  
  /** These are the methods used when building a Data.
   */
  trait EltCtx {
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
  /*-----------------------------------------------------------------------*/
  /*       SECTION II: Define DefImpl                                      */
  /*-----------------------------------------------------------------------*/
  
  /** We make a minimal common implementation for ctx and ext.
   */
  trait DefImpl extends super.DefImpl {self=>
    type Value      = String
    type Key        = String
    type Ret        = Unit
    type BaseParser = ParserBuilder //any parser
    //type UCtx[-p<:BaseParser]>:Null<:CtxCore.UsrCtx[p,this.type] with ObjectMotor.UCtx[p,this.type]
    final def baseParserClass = classOf[BaseParser]
    
    val noKey = ""

    def info(e:Elt):Info
      
    /**
     * @param on, the object to fill up
     */    
    abstract class DlgBase(on:AnyRef) extends super.DlgBase {this:Dlg=>
      type Result = AnyRef  
      //pretty simple processor here: actual complexity is in Data
      //for efficiency, the common implementation is minimal
      def onInit(e:Elt):Unit           = {}
      def onBeg(e:Elt): Unit           = {}
      def onChild(e:Elt,child: Elt, r: Unit): Unit = {}
      def onInit():Unit = {}
      def onExit():AnyRef = on
    }
  }
      
  /*-----------------------------------------------------------------------*/
  /*       SECTION III: Define ctx implementation                          */
  /*-----------------------------------------------------------------------*/
  
  /** Actual implementation for CtxCore.
   */
  object ctx extends loader.core.CtxCore.Abstract[Data] with DefImpl {self=>
    override type Data                 = ObjectMotor.Data
    override type UCtx[-p<:BaseParser] = CtxCore.UsrCtx[p,this.type] with ObjectMotor.UCtx[p,ctx.type] 
    final def baseUCtxClass   = null //XXX classOf[UCtx[_]] => use TypeTags for dynamic class checking
    def info(e:Elt) = new Info {
      def eltCtx                      = e.eltCtx
      def fd:AutoConvertData          = e.fd
      def isSeq:Boolean               = e.fd.isSeq
      def depth:Int                   = e.fd.depth
      def eClass                      = e.eClass
    }
    
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase {
      def getData(e:Elt):Data         = if (e.parent==null) new RootData(on) else e.parent.data(e.name,info(e))
      def onVal(e:Elt,v:String): Unit = e.data.set(v)           //set terminal field
      def onEnd(e:Elt): Unit          = e.data.close(e.eltCtx)  //'close' container (list/struct) and assign result to upper layer
    }
    def apply(pr: utils.ParamReader):Dlg   = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }
  
  /** Actual implementation for EtxCore.
   *  As we have no context, we will have to infer the properties of each element.
   *  This processor can spawn any class.
   */
  object ext extends loader.core.ExtCore.Abstract[Data] with DefImpl {self=>
    override type Data                 = ObjectMotor.Data
    override type UCtx[-p<:BaseParser] = ObjectMotor.UCtx[p,ext.type] 
    final def baseUCtxClass   = null //XXX classOf[UCtx[_]] => use TypeTags for dynamic class checking
    protected[this] val dummy = new AutoConvertData {  //no data for conversions
      def convert: String = ""
      def check: String = ""
      def param: String = ""
      def valid: String = ""
    }
    protected[this] var s:ctx.Status = _  //the last evaluated status
    def info(e:Elt) = new Info {
      def eltCtx             = e.eltCtx
      val fd:AutoConvertData = dummy   //no data
      val isSeq:Boolean      = false   //no sequences
      val depth:Int          = 0       //inferred
      def eClass:Int         = s.kind  //not beautiful, eh ? price to pay for better perfs by using ext which is not really designed to handle this case...
    }
    
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase {
      def getData(e:Elt):Data          = if (e.parent==null) new RootData(on) else e.parent.data(e.name,info(e))
      def onVal(e:Elt,v:String): Unit  = e.data.set(v)           //set terminal field
      def onEnd(e:Elt): Unit           = e.data.close(e.eltCtx)  //'close' container (list/struct) and assign result to upper layer
    }
    def apply(pr: utils.ParamReader):Dlg = ???
    def apply(on:AnyRef):Dlg = new Dlg(on:AnyRef)
  }

  /** This cannot be implemented: we have to maintain a context. */
  def cre = ???
  
  /** We define a specific UserType here.
   *  This processor indeed requires specific information to work, and these can be tuned by the end user.
   *  see ObjectMotor.EltCtx.
   */
  trait UCtx[-P<:ParserBuilder,-M<:DefImpl] extends UsrCtx[P,M] {
    type EltCtx>:Null<:EltCtxBase
    protected[this] trait EltCtxBase extends super.EltCtxBase with ObjectMotor.EltCtx { this:EltCtx=>
    }
  }
  
  /** This trait can be placed on the UsrCtx#EltCtx class to enable tag "annotations" inference when
   *  either or both depth/loader are not filled up. Not using it forces the user to have fully filled tags.
   *  The drawback here is that inference has a performance cost, and in rare cases it might not ensure the
   *  expected result (in all tests it does!)
   */
  trait CtxStatusInferrence[P<:ParserBuilder] extends ECtx[P,ctx.type] {
    abstract override def onName(key:elt.Key0):ctx.Status = Inference(elt,super.onName(key))
  }
  trait CtxFdInferrence[P<:ParserBuilder] extends ECtx[P,ctx.type] {
    abstract override def onName(key:elt.Key0):ctx.Status = {
      import ParserBuilder.{ skip, skipEnd }
      try { super.onName(key) } catch {
        case e@ (`skip` | `skipEnd`) => try {
          val fd = Inference(elt,key,elt.fd.ctx(key))        //read field and infer state
          elt.asInstanceOf[CtxCore#Struct].tags.put(key,fd)  //dynamically assign the new field to the tagMap ; if the cast fails, we indeed have an error.
          super.onName(key)                                  //return appropriate status
        } catch {
          case _:Throwable => throw e
        }
      }
    }
  }
  //a shortcut for full inference, which is what is usually wanted ; note that the order of the traits is important here.
  trait CtxFullInfer[P<:ParserBuilder] extends CtxFdInferrence[P] with CtxStatusInferrence[P]
  
  protected def readParams(pr: utils.ParamReader) = ???

  /** Logic for inferring missing data in FieldContext.
   */
  object Inference {
    //we don't believe the upper layer for terminals or lists ; the default impl relies on fd, but it may be uncomplete
    //if the user uses defaults. Possibly we have to guess by watching the actual bound field.
    //X being the field type, either we have a converter String -> X and X can be terminal, or we don't.
    //In that case, we assume a struct with X used to load.
    def apply(elt:ctx.Elt,s:ctx.Status):ctx.Status = {
      //BEWARE: the next lines are extremely delicate.
      if (s.kind!=CtxCore.struct && elt.eClass==CtxCore.struct) {
        val da = DataActor(elt.data.asInstanceOf[StcData].on.getClass,s.key,"bsfm").get  //find field; the cast will work because there is a bijection StcData <-> Struct
        val delta = if (s.fd.isSeq) 1 else 0                                             //depth offset accounting for sequences (which look like one level less)
        val n = s.fd.depth+delta                                                         //calculate depth for analysis: result is <=0 for undeclared depth, >0 for declared depth
        val r = Reflect.analyzeType(da.expected,elt.eltCtx.converters,n) match {         //analyze its type
          case (`n`,None) => return s                                                    //the declared depth matches and we can convert the end type: believe what we have
          case (i,x) =>
            x match {
              case None    => (i>delta,s.fd.rebuild(i),false)                                        //infer depth.
              case Some(c) => (i>delta,s.fd.rebuild(Reflect.findClass(c).getName,s.fd.isSeq,i),true) //infer depth/loader
            }
        }
        return new CtxCore.Status(s.key,s.idx,r._2,s.broken,if (s.kind==CtxCore.list || r._1) CtxCore.list else if (r._3) CtxCore.struct else CtxCore.term)
      }
      s
    }
    //this will be used to infer a Context#FieldMapping from a underlying field/method.
    //this is used when we don't even want to place some/all annotations.
    //Note that ALL types that get an answer from the collection solver will be assumed to be lists.
    //As a consequence, no sequences are ever inferred. Not wanting to write anything has a cost...
    def apply(elt:ctx.Elt,key:ctx.Key,empty:Context#FieldMapping):Context#FieldMapping = {
      val da = DataActor(elt.data.asInstanceOf[StcData].on.getClass,key,"bsfm").get
      val r = Reflect.analyzeType(da.expected,elt.eltCtx.converters,0)
      r._2 match {
        case None    => empty.rebuild(null,false,r._1)
        case Some(c) => empty.rebuild(Reflect.findClass(c).getName,false,r._1)
      }
    }  
  }
    //we don't believe the upper layer for terminals or lists ; the default impl relies on fd, but it may be uncomplete
    //if the user uses defaults. Possibly we have to guess by watching the actual bound field.
    //X being the field type, either we have a converter String -> X and X can be terminal, or we don't.
    //In that case, we assume a struct with X used to load.
    def apply(elt:ext.Elt,s:ctx.Status):ctx.Status = {
      //BEWARE: the next lines are extremely delicate.
      if (s.kind!=CtxCore.struct && elt.data.isInstanceOf[StcData]) {
        val da = DataActor(elt.data.asInstanceOf[StcData].on.getClass,s.key,"bsfm").get  //find field; the cast will work because there is a bijection StcData <-> Struct
        val delta = if (s.fd.isSeq) 1 else 0                                             //depth offset accounting for sequences (which look like one level less)
        val n = s.fd.depth+delta                                                         //calculate depth for analysis: result is <=0 for undeclared depth, >0 for declared depth
        val r = Reflect.analyzeType(da.expected,elt.eltCtx.converters,n) match {         //analyze its type
          case (`n`,None) => return s                                                    //the declared depth matches and we can convert the end type: believe what we have
          case (i,x) =>
            x match {
              case None    => (i>delta,s.fd.rebuild(i),false)                                        //infer depth.
              case Some(c) => (i>delta,s.fd.rebuild(Reflect.findClass(c).getName,s.fd.isSeq,i),true) //infer depth/loader
            }
        }
        return new CtxCore.Status(s.key,s.idx,r._2,s.broken,if (s.kind==CtxCore.list || r._1) CtxCore.list else if (r._3) CtxCore.struct else CtxCore.term)
      }
      s
    }
}

