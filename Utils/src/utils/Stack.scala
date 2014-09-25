package utils

/**
 * A simple Stack class, based on arrays for speed.
 * It is covariant ; the +=, arrayCopy methods would break that rule.
 * However, the underlying arrays ensures the correct type is actually checked.
 * We want this check to ensure that we are actually using the excepted types.
 * This is one of the rare place where we can actually check that our reflection
 * API is not broken.
 */
class Stack[+X](cx:Class[X],private var sz:Int=200) {
  if (sz<=0) sz=10
  private[this] var table = alloc
  protected var idx=0
  
  private[this] def alloc:Array[X] = {
    val r=java.lang.reflect.Array.newInstance(cx,sz).asInstanceOf[Array[X]]
    if (table!=null) arrayCopy(r)
    r
  }
  protected def check()          = if (idx==table.length) { sz+=sz; table=alloc }
  def +=(x:Any)                  = { check; table(idx)=x.asInstanceOf[X]; idx+=1 }
  def foreach(f:X=>Unit)         = if (idx>0) { var i=0; do { f(table(i)); i+=1} while(i<idx) } 
  def arrayCopy(b:Array[_]):Unit = System.arraycopy(table,0,b,0,idx)
  def clear()                    = idx=0
  def length                     = idx
  def apply(i:Int)               = table(i)
  def getEltType                 = table.getClass.getComponentType
  protected def inner:Array[_]   = table
}