package utils

class Indent(val n:Int) {
  def apply(step:Int) = new Indent(n+step)
  override def toString = Indent(n)
}
object Indent {
  val base = "\n                                                                                                              "+
             "                                                                                                                "+
             "                                                                                                                "+
             "                                                                                                                "+
             "                                                                                                                "
  def apply(n:Int) = base.substring(0,n+1)
  def apply(n:Int,p:Int) = base.substring(0,n*p+1)
}