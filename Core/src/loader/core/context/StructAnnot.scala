package loader.core.context

/** A type that describes our struct level annotations
 *  Required because Annotations don't have class hierarchies!
 *  And conf files can't ever be Annotations (which is not instanciable in the program)
 *  Acts as a common interface.
 *  Each context must provide a way to recover this info. 
 */
abstract class StructAnnot {
  def auditMin:Int
  def auditMax:Int
  def fast:Boolean
  def clzz:Class[_<:AnyRef]  //the class attached to this structure in case we want to build an object
  final def asXmlAttrib(id:String):String = {
    s"id='$id' auditMin='$auditMin' auditMax='$auditMax' fast='$fast' ${if(id!=clzz.getName)s" clzz='${clzz.getName}'"else""}"
  }
}