package loader.core.context

/** A local utility to write contexts as Xml
 */
private object XmlHelper {
  def t(x:Int) = utils.Indent(x+x)
  def v(nm:String,v:String)  = if (v!=null && v.length>0) s" $nm='$v'"   else ""
  def v(nm:String,v:Boolean) = if (v)                     s" $nm='true'" else ""
  def v(nm:String,v:Int,default:Int) = if (v!=default)    s" $nm='true'" else ""
}
