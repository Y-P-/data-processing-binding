package parser.px.analyzers.structured
import scala.collection.mutable.HashMap

/**
 * Utility class for referencing a field by it's full path.
 * The class keeps track of all intermediary objects.
 */
protected class Binder(val parent:Binder, val fld:Field) {
  def ->>(name:String)       = Binder(this,name,0)
  def ->>(name:(String,Int)) = Binder(this,name._1,name._2)
  def ->>(idx:Int)           = Binder(this,fld.name,idx)
  /** Inserts a field at the current field position ; the current field (and following) are moved */
  def <==(fld:Field):Unit = parent.fld.insertAt(this.fld,fld)
  /** Inserts a field after the current field ; the next fields are moved */
  def <=+(fld:Field):Unit = parent.fld.insertAfter(this.fld,fld)
  /** Replaces the current field */
  def <<=(fld:Field):Unit = parent.fld.replace(this.fld,fld)
  /** Deletes the field */
  def delete():Unit       = parent.fld.delete(fld)
  
  override def toString = fld.toString
}

object Binder {
  import scala.language.implicitConversions
  implicit def toInt(b:Binder)       = b.fld.toInt
  implicit def toDouble(b:Binder)    = b.fld.toDouble
  implicit def toBool(b:Binder)      = b.fld.toBool
  implicit def toFld(b:Binder)       = b.fld
  implicit def toBinder(f:Container) = this(f)
  implicit def toCouple(s:String)    = new { def apply(idx:Int)=(s,idx) }
    
  def apply(root:Container):Binder =
    new Binder(null,root.asInstanceOf[Field])
  protected def apply(parent:Binder, name:String, idx:Int):Binder = parent.fld match {
    case c:Container => new Binder(parent,c(name,idx))
    case x           => throw new IllegalStateException("cannot fetch field <"+name+"> from leaf field <"+x.name+">")
  }
}