package loader.audit

object IdScheme {
  
  val core = new BaseScheme[loader.core.definition.Processor] {
    protected def build(ld:Elt):StringBuffer = {
      val b = new StringBuffer
      ld.foreach { x:Elt=>
        if (b.length>0) b.append(".")
        b.append(x.name)
      }
      b
    }
  }
  val ctx = new BaseScheme[loader.core.CtxCore.Processor] {
    protected def build(ld:Elt):StringBuffer = {
      val b = new StringBuffer
      ld.foreach { x:Elt=>
        if (b.length>0) b.append(".")
        b.append(x.rankedName)
      }
      b
    }
  }
  
  abstract class BaseScheme[-P<:loader.core.definition.Processor] extends IdentifierScheme[P] {
    protected[this] final type Elt = P#Element  //for easy reference when defining concrete classes
    def apply(ld:Elt):String = build(ld).toString
    def apply(ld:Elt,name:String):String = add(build(ld),name).toString
    protected def build(ld:Elt):StringBuffer
    protected def add(b:StringBuffer, name:String):StringBuffer = name match {
      case null => b
      case _    => (if (b.length>0) b.append(".") else b).append(name)
    }  
  }
}