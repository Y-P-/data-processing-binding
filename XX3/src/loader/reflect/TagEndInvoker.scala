package loader.reflect

import java.lang.reflect.{Modifier,Method}
import loader.{InternalLoaderException,Element}
import loader.annotations.TagEnd

/** A class used to invoke the method used to return the loader value upon completion.
 */
class TagEndInvoker protected (v1:(AnyRef)=>Any,v2:(AnyRef,Element)=>Any,val returnedType:Class[_]) {
  def this(v:(AnyRef)=>Any,expectedType:Class[_])        = this(v,null,expectedType)
  def this(v:(AnyRef,Element)=>Any,expectedType:Class[_]) = this(null,v,expectedType)
  def apply(on:AnyRef,ld:Element):Any = if (v1!=null) v1(on) else if (v2!=null) v2(on,ld) else on
}

object TagEndInvoker {
  /** Utility to easily build the findTagEnd method in concrete classes.
   *  @param  czL, the loading helper class
   *  @param  isTagEnd, a method that identifies the appropriate tagEnd method
   *  @return the unique tagEnd method to use on the loaded item to return it's value to the upper layer
   */
  def apply(czL:Class[_],isTagEnd:(Method)=>Boolean):TagEndInvoker = {
    var r:TagEndInvoker = null
    def analyze(m:Method) = {
      if (r!=null) throw new InternalLoaderException("Only one tagEnd allowed: "+czL)
      val returnedType = m.getReturnType
      r = if (m.getParameterTypes.length==0)                                                    new TagEndInvoker(m.invoke(_:AnyRef),returnedType)
          else if (m.getParameterTypes.length==1 && m.getParameterTypes()(0)==classOf[Element]) new TagEndInvoker(m.invoke(_:AnyRef,_:Element),returnedType)
          else throw new InternalLoaderException("Badly placed TagEnd annotation (only method with no argument or Loader argument)")      
    }
    //protected/private local methods
    //note that for some reason Scala adds a second method returning AnyRef ; we discard it.
    for (m <- czL.getDeclaredMethods if (!Modifier.isPublic(m.getModifiers) && isTagEnd(m) && m.getReturnType!=classOf[AnyRef]))
      analyze(m)
    //public methods (incl. inherited)
    //note that for some reason Scala adds a second method returning AnyRef ; we discard it.
    for (m <- czL.getMethods if (isTagEnd(m) && m.getReturnType!=classOf[AnyRef]))
      analyze(m)
    r
  }
  /** The common standard way to find tag end:
   *  - use the given name unless null or empty
   *  - use the TagEnd annotated method
   *  - use identity
   */
  def apply(czL:Class[_],name:String):TagEndInvoker = {
    val r = if (name!=null && !name.isEmpty) TagEndInvoker(czL,_.getName==name)
            else                             TagEndInvoker(czL,_.getAnnotation(classOf[TagEnd])!=null)
    if (r!=null) r else new TagEndInvoker(null,null,czL)
  }
}
