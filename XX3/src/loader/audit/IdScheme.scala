package loader.audit

object IdScheme {
  
  val core = new BaseScheme[loader.core.definition.Def#Elt] {
    protected def build(ld:Element):StringBuffer = {
      val b = new StringBuffer
      ld.foreach { x:Element=>
        if (b.length>0) b.append(".")
        b.append(x.name)
      }
      b
    }
  }
  val ctx = new BaseScheme[loader.core.CtxCore.Def#Elt] {
    protected def build(ld:Element):StringBuffer = {
      val b = new StringBuffer
      ld.foreach { x:Element=>
        if (b.length>0) b.append(".")
        b.append(x.localName)
      }
      b
    }
  }
  
  abstract class BaseScheme[E<:loader.core.definition.Def#Elt] extends IdentifierScheme[E] {
    type Element = E
    def apply(ld:Element):String = build(ld).toString
    def apply(ld:Element,name:String):String = add(build(ld),name).toString
    protected def build(ld:Element):StringBuffer
    protected def add(b:StringBuffer, name:String):StringBuffer = name match {
      case null => b
      case _    => (if (b.length>0) b.append(".") else b).append(name)
    }  
  }
}