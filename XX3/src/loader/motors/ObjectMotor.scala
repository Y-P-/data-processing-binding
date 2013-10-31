package loader.motors.old

import loader._
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