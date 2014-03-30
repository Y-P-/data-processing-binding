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
    def apply(kind:Int, name:String, info:Info):Data = ??? //data factory for a sub-object, obviously an error for Terminal
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
    override def apply(kind:Int, name:String, info:Info):Data = Data(kind,info.eltCtx,b)
  }
  private class StcData (val on:AnyRef,b:Binder#I,private[this] var seqs:Map[String,Binder#I]) extends Data(b) {
    protected final def localClose() = for (x <- seqs) x._2.close()    //close all current sequences
    override def close(eltCtx:EltCtx) = { localClose(); set(on) }  //and assign structure to its owner
    override def apply(kind:Int, name:String, info:Info):Data = {
      val b = Binder(DataActor(on.getClass,name,"bsfm").get, info.eltCtx.converters, info.fd, info.depth>0)(on)
      Data(kind, info.eltCtx,  //seqs are kept in an immutable map (we expect it to stay small) stored in a local var which we update when needed 
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
    def eltCtx:EltCtx        //element context of element we are building the data for
    def fd:AutoConvertData   //data for auto converting the element we are building
    def isSeq:Boolean        //is the element being built part of a sequence ?
    def depth:Int            //if element being built is a list, depth
  }
  
  /** These are the methods used when buildind a Data.
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
  
  /** We make a common implementation for ctx and ext, even though it is likely marginally less efficient
   *  It is a bit more complex too, but this makes only one algorithm to maintain.
   */
  trait DefImpl extends super.DefImpl with CtxCore {self=>
    type Value      = String
    type Key        = String
    type Ret        = Unit
    type BaseParser = ParserBuilder //any parser
    type Data       = ObjectMotor.Data
    type UCtx[-p<:BaseParser]>:Null<:CtxCore.UsrCtx[p,this.type] with ObjectMotor.UCtx[p,this.type]
    final def baseParserClass = classOf[BaseParser]
    
    val noKey = ""

    def info(e:Elt):Info
      
    /**
     * @param on, the object to fill up
     */    
    abstract class DlgBase(on:AnyRef) extends super.DlgBase {this:Dlg=>
      type Result = AnyRef
      type EX = Elt
      
      //building the data: we must first examine the kind of parent we have
      def getData(e:Elt):Data = if (e.parent==null) new RootData(on) else e.parent.data(e.eClass,e.name,info(e))
  
      //pretty simple processor here: actual complexity is in Data
      def onInit(e:Elt):Unit           = {}
      def onBeg(e:Elt): Unit           = {}
      def onVal(e:Elt,v:String): Unit  = e.data.set(v)           //set terminal field
      def onEnd(e:Elt): Unit           = e.data.close(e.eltCtx)  //'close' container (list/struct) and assign result to upper layer
      def onChild(e:Elt,child: Elt, r: Unit): Unit = {}

      def onInit():Unit = {}
      def onExit():AnyRef = on
    }
  }
  
  /*-----------------------------------------------------------------------*/
  /*       SECTION III: Dynamic Field analysis, Dynami fd building         */
  /*-----------------------------------------------------------------------*/
  
  /** Analyzes a type to determine if it is a collection, and in that case the relevant information.
   * @param t, the type to analyze
   * @param cv, the conversion solver in use
   * @param n, the depth for the analysis (0 is all the way to the bottom of encapsulated seqs/lists)
   * @returns  the actual depth if less than n, then None if the type can be converted or Some(class found)
   */
  def analyzeType(t:java.lang.reflect.Type, cv:ConversionSolver, n:Int):(Int, Option[java.lang.reflect.Type]) = {
    import Reflect._
    val l = cv.collectionSolver(t)
    if (l!=null) {
      val x = l.depth(n)
      val isConvertible = cv.stringSolver(x._2.czElt)
      println(x)
      (x._1, if (isConvertible==None) Some(x._2.czElt) else None)
    } else {
      val isConvertible = cv.stringSolver(t)
      (0,if (isConvertible==None) Some(t) else None)
    }
  }
  
  /** Rebuilds a fd using a specified loader.
   *  This is usually called when the initial fd loader was blank (i.e. left for inferrence.)
   */
  def rebuild(fd:Context#FieldMapping, loader0:String, isSeq0:Boolean, depth0:Int):Context#FieldMapping = {
    val annot = new loader.core.context.FieldAnnot {
      def inName:String      = fd.annot.inName
      def loader:String      = loader0
      def isSeq:Boolean      = isSeq0
      def depth:Int          = depth0
      def contiguous:Boolean = fd.annot.contiguous
      def min:Int            = fd.annot.min
      def max:Int            = fd.annot.max
      def audit:String       = fd.annot.audit
      def check:String       = fd.annot.check
      def valid:String       = fd.annot.valid
      def param:String       = fd.annot.param
      def convert:String     = fd.annot.convert
    }
    new fd.ctx.FieldMapping(annot)
  }
  
  /*-----------------------------------------------------------------------*/
  /*       SECTION IV: Define ctx implementation                           */
  /*-----------------------------------------------------------------------*/
  
  /** Actual implementation for CtxCore.
   */
  object ctx extends loader.core.CtxCore.Abstract[Data] with DefImpl {self=>
    override type Data                 = ObjectMotor.Data
    override type UCtx[-p<:BaseParser] = CtxCore.UsrCtx[p,this.type] with ObjectMotor.UCtx[p,ctx.type] 
    final def baseUCtxClass   = null //XXX classOf[UCtx[_]] => use TypeTags for dynamic class checking
    def info(e:Elt) = new Info {
      def eltCtx              = e.eltCtx
      def fd:AutoConvertData  = e.fd
      def isSeq:Boolean       = e.fd.isSeq
      def depth:Int           = e.fd.depth
    }
    class Dlg(on:AnyRef) extends DlgBase(on) with super[Abstract].DlgBase {
      override def onName(parent:Elt,key:Key):Status = {
        val s=super.onName(parent,key)
        return s
        //we don't believe the upper layer for terminals ; the default impl relies on fd, but it may be uncomplete
        //if the user uses defaults. Possibly we have to guess by watching the actual bound field.
        //X being the field type, either we have a converter String -> X and X can be terminal, or we don't.
        //In that case, we assume a struct with X used to load.
        if (s.kind!=CtxCore.struct && parent.eClass==CtxCore.struct) {
          val da = DataActor(parent.data.asInstanceOf[StcData].on.getClass,s.key,"bsfm").get
          s.kind match {
            case CtxCore.list =>
              val n = s.fd.depth+(if (s.fd.isSeq) 1 else 0)
              analyzeType(da.expected,parent.eltCtx.converters,n) match {
                case (_,None)    =>  //OK, can be converted : don't change anything
                case (i,Some(x)) =>  //Can't be converted
                   println(s"+${s.key} ${if (s.fd.loader!=null) s.fd.loader.id else "<>"} ${s.fd.depth}")
                   val s1 = new CtxCore.Status(key,s.idx,
                     rebuild(s.fd,Reflect.findClass(x).getName,s.fd.isSeq,i-(if (s.fd.isSeq) 1 else 0)),
                     s.broken,
                     CtxCore.list)
                   println(s"+${s.key} ${s1.fd.loader.id} ${s1.fd.depth}")
                   return s1
              }
            case CtxCore.term =>
              val n = s.fd.depth+(if (s.fd.isSeq) 1 else 0)
              analyzeType(da.expected,parent.eltCtx.converters,n) match {
                case (_,None)    =>  //OK, can be converted : don't change anything
                case (i,Some(x)) =>  //Can't be converted
                   println(s"*${s.key} ${if (s.fd.loader!=null) s.fd.loader.id else "<>"} ${s.fd.depth}")
                   val s1 = new CtxCore.Status(key,s.idx,
                     rebuild(s.fd,Reflect.findClass(x).getName,s.fd.isSeq,i-(if (s.fd.isSeq) 1 else 0)),
                     s.broken,
                     CtxCore.struct)
                   println(s"*${s.key} ${s1.fd.loader.id} ${s1.fd.depth}")
                   return s1
              }
          }
        }
        s
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
    protected[this] trait EltCtxBase extends super.EltCtxBase with ObjectMotor.EltCtx { this:EltCtx=>
    }
  }
  
  protected def readParams(pr: utils.ParamReader) = ???
}
