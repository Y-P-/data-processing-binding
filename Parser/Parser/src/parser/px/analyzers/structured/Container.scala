package parser.px.analyzers.structured
import scala.collection.immutable.Stack

/**
 * Defines common methods for Container like objects (Fields containing other fields).
 * This doesn't preclude any specific implementation for the actual class.
 * Override the appropriate version of idexOf to get best performances depending on
 * your encoding and appropriate way to access field names. If you don't, this will
 * still work, but with maybe needless conversions.
 */
protected trait Container extends Field with Seq[Field] {
  override def deep[R](f:(Field, =>Unit)=>R):R = f(this,foreach(_.deep(f)))
  override def deep2[R](stack:Stack[Field]=new Stack[Field])(f:(Field, =>Unit, Stack[Field])=>R):R = f(this,foreach(_.deep2(stack.push(this))(f)),stack)
  override def deep3[R](r:R,f:(R, Field, (R)=>R)=>R):R = { var r0=r; foreach(fld=>r0=f(r0,fld,fld.deep3(_,f))); r0}
  final def value:Nothing = throw new IllegalStateException("requesting value for a container field")
  final override def :=(newValue:String):Nothing = throw new UnsupportedOperationException("setting the value of a container field")

  /** Find index of the idx'th field of the given name ; works for anonymous fields */
  def indexOf(name:String,idx:Int):Int = {
    if (idx<0 || idx>=length)                 throw new IndexOutOfBoundsException(idx.toString)
    if      (length==0)                       -1
    else if (this(0).isInstanceOf[Anonymous] || (name.length==0)) idx
    else                                      { var i=idx; indexWhere{f => if (f.isName(name)) i-=1; i<0} }
  }
  def indexOf(name:String):Int           = indexOf(name,0)
  def apply(name:String,idx:Int):Field   = try {
      this(indexOf(name,idx))
    } catch {
      case e:Throwable => throw new IllegalStateException("field <"+name+">("+idx+") could not be found ; check name and index")
    }
  def apply(name:String):Field           = this(indexOf(name,0))
  /** Creates a view on a field, returning a non strict sequence of fields having the same name */
  def select(name:String) = view.filter(_.isName(name))
  /** Finds the index of the exact field fld or raises an exception */
  protected final def find(fld:Field):Int = {
    val idx = indexWhere(_ eq fld)
    if (idx<0) throw new IllegalStateException("field "+fld.name+" not found in "+name)
    idx
  }
   
  /** The common String representation for the field. Beware, this may be very large for top objects! */
  override def toString = {
    var sb = new StringBuffer
    deep { (f,recurse) => sb.append(f.leftString); recurse; sb.append(f.rightString).append(" ") }
    sb.toString
  }
  def leftString  = "{"
  def rightString = "}"
}

object Container {
  import scala.language.implicitConversions
  implicit def toBinder(c:Container):Binder = Binder(c)  
}
  
protected trait Root extends Container {
  abstract override def rightString:String = ""
  abstract override def leftString:String  = ""
}