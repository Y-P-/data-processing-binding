package utils

/** A small utility to ease indentations.
 *  @param n, the indentation depth
 *  @param first, true if this is the first Indent element, for which we don't put a newline.
 */
class Indent(val n:Int,first:Boolean) {
  def this(n:Int) = this(n,true)
  def apply(step:Int) = new Indent(n+step,false)
  override def toString = Indent(n,first)
}
object Indent {
  val base = "\n                                                                                                              "+
             "                                                                                                                "+
             "                                                                                                                "+
             "                                                                                                                "+
             "                                                                                                                "
  def apply(n:Int,first:Boolean)       = base.substring(if (first)1 else 0,n+1)
  def apply(n:Int,p:Int,first:Boolean) = base.substring(if (first)1 else 0,n*p+1)
}