package loader.motors

/*
/**
 * @param root, a method that gives an initial item to work on
 */
class ObjectMotor[+R](val root:()=>AnyRef) extends Motor[R,AnyRef] with Cloneable {
  final def apply(stc:StructField):RootEngine[R] = stc.stk.auditRecorder match {
    case null => new ObjectEngine(root(),stc)
    case a    => new AuditedObjectEngine(root(),stc,a.audit)
  }
  def apply(ld:Loader,pr:utils.ParamReader):EngineBuilder[AnyRef] =
    new ObjectMotor(()=>ld.binding.newItem)
}

/** The Engine without auditing capabilities */
private final class ObjectEngine[+R](override val item:AnyRef, override val stc:StructField) extends BaseObjectEngine(stc,item) with RootEngine[R] {
  final def builder(stc:StructField,parent:E) = new BaseObjectEngine(stc,parent.getChildItem(stc),parent) with Sub
}

/** The Engine with auditing capabilities */
private final class AuditedObjectEngine[+R](override val item:AnyRef, override val stc:StructField, val audit:Auditer) extends BaseObjectEngine(stc,item) with AuditedRootEngine[R] {
  final def builder(stc:StructField,parent:E) = new BaseObjectEngine(stc,parent.getChildItem(stc),parent) with Sub
}

protected abstract class BaseObjectEngine(val stc:StructField,val item:AnyRef,val parent:BaseObjectEngine) extends Engine { self=>
  final protected val loader:Context#StructMapping = stc.fd.loader
  final type Stc = AnyRef
  def this(stc:StructField,item:AnyRef) = this(stc,stc.getItemOrElse(item),stc.getParentOrNull[BaseObjectEngine])
  final type E = BaseObjectEngine
  final def getChildItem(child:StructField):AnyRef = {     //read preexisting field or create brand new one
    val b = child.binding
    val x = b.get(item).asInstanceOf[AnyRef]
    if (x==null) b.newItem else x
  }
  final def parentMotor = stc.parent.motor.asInstanceOf[this.type]  //only one engine active in a given stack!
  //List elements are NOT named ; only the whole list can be.
  //Doing otherwise requires a deeper analysis of the generic declaration, and there really is little gain.
  def onNewElt(ld:Loader):Unit              = ()
  final override def getSeq(ld:Loader):Seq  = ld.binding.seq(ld.fd)
  def onString(fld:Loader,value:String):Any = fld.binding.namedString(fld,value)
  def onListEnd(lst:ListField):Any          = lst.binding.namedAny(lst,onSeqEnd(lst,lst.seq))
  final override def onValidStr(ld:Loader)  = identity[String]  //check is done on assignment by the binder cv method
  def onStructEnd():Any = {
    if (item.isInstanceOf[Named]) item.asInstanceOf[Named].name_=(stc.name)
    stc.endSeqs
    if (parent!=null) stc.binding.namedStruct(stc,item)  //send completed data to parent
    else              item                               //stc.fd.binding(null).finalValue(item)    would invoke terminal tagEnd, but it doesn't seem smart to do so: leave it to the user
  }
  final def onInit(ld:Loader):Unit = ()
  override def onStcField(child:Loader,value:Any) = child.binding.set(item,value)
}
*/

import java.io.{Writer,FileWriter,File,OutputStreamWriter}
import loader.core._
import loader.core.definition._
import loader.core.ParserBuilder
import loader.reflect._
import loader.core.context.Context

object ObjectMotor extends ProcessorImpl {
  /*
  class Data
  CASE 1:  Stc in Stc
    build new instance OR fetch existing one
    build Binder for field
  CASE 2:  Stc in Lst
    build new Instance
    use Lst Binder subInstance
  CASE 3:  Stc in Seq
    build new Instance
    use Binder subinstance fetched from map OR build it and put it in map
  CASE 4:  Lst in Stc
    build Binder subinstance for field
  CASE 5:  Lst in Lst
    build Binder subinstance from upper list subinstance
  CASE 6:  Lst in Seq
    build Binder subinstance from upper list subinstance fetched from map (create it first if doesn't exist)
  CASE 7:  Seq in Stc
    use Binder subinstance fetched from map OR build it and put it in map
  CASE 8:  Seq in Lst
    nothing (nominal situation)
  CASE 9:  Ter in Stc
    build Binder for field
  CASE 10: Ter in Lst
    use Lst Binder subInstance
  CASE 11: Ter in Seq
    use Binder subinstance fetched from map OR build it and put it in map
  
  - seq are only found in stc
  - 
  
  class StcData(val o:AnyRef,val b:Binder[CtxCore#Elt]#I) extends Data {
    def this(e:CtxCore#Elt) {
      val d = if (e.parent.fd.isStruct) DataActor(e.parent.data.o.getClass,e.name,"bsfm").get      
    }
    val seqs = scala.collection.mutable.HashMap.empty[String,Binder[CtxCore#Elt]#I]
  }
  class LstData
   
  */
  class Data(val b:Binder[CtxCore#Elt]#I)
  class StcData(val o:AnyRef,b:Binder[CtxCore#Elt]#I,val seqs:scala.collection.mutable.HashMap[String,Binder[CtxCore#Elt]#I]) extends Data(b)
  
  object Data {
    def apply(e:CtxCore#Elt,i:Binder[CtxCore#EltBase]#I) = e match {
      case _:CtxCore#Terminal =>
        new Data(i)
      case _:CtxCore#Struct =>
        //we need: a new object for the structure, the binder to attach that object to the parent, a map to manage sequences
        new StcData(i.eltClass.asInstanceOf[Class[_<:AnyRef]].newInstance,i,scala.collection.mutable.HashMap.empty)
      case _:CtxCore#List =>
        new Data(i.subInstance)
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
     * @param out, where to write to
     * @param indent, indent value as space number ; 0 means all output on one line
     */    
    abstract class DlgBase(val on:AnyRef) extends super.DlgBase {this:Dlg=>
      type Result = AnyRef
      def getData(e:Elt):Data = e.parent match {
        //this is the top object we are filloing/building
        case null => new StcData(on,null,scala.collection.mutable.HashMap.empty)
        //parent is an object: we must bind one of its field
        case s:Struct =>
          val d0 = s.data.asInstanceOf[StcData]
          val o = d0.o
          if (e.fd.isSeq)  //for a seq, find or create the appropriate binder
            Data(e,d0.seqs.getOrElseUpdate(e.name, Binder(DataActor(o.getClass,e.name,"bsfm").get, StandardSolver(), e.fd, true)(o)))
          else             //otherwise, bind the field
            Data(e,Binder(DataActor(o.getClass,e.name,"bsfm").get, StandardSolver(), e.fd, e.fd.isList)(o))
        case l:List => Data(e,l.data.b)
      }
  
      def onInit(e:Elt):Unit           = {}
      def onBeg(e:Elt): Unit           = {}
      def onVal(e:Elt,v: String): Unit = e.data.b.set(v,e)
      def onEnd(e:Elt): Unit           = if (e.parent!=null) e.data.b.close(e)
      def onChild(e:Elt,child: Elt, r: Unit): Unit = child match {
        case _:Terminal =>
        case _:List     => 
        case _:Struct   => child.data.b.set(child.data.asInstanceOf[StcData].o,child)
      }

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
