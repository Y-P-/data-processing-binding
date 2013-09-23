package loader.features

import java.io.File
import java.net.URI
import loader.exceptions.{InvalidConversionException,InternalLoaderException}
import loader.context.ClassContext
import java.net.URLEncoder
import loader.core.CtxCore.Def

abstract class DegenHandler {
  /**
   * Handles a degenerated form of a structure (as a String)
   * @param stc, the degenerated structure
   * @param value, it's degenerated String form
   * @return the value for the degen struct
   */
  def apply(stc:Def#Struct,value:String):Any
  /**
   * true if this is an include handler (mostly for DOM handling)
   */
  def isInclude:Boolean = false
  /**
   * Uri value for an include handler
   */
  def getUri(value:String):URI = throw new IllegalStateException
}

/** class used to indicate that a degen handler does not return any value and should be ignored */
final object IgnoreDegenException extends InternalLoaderException("",null,null)


object convertDegenHandler extends DegenHandler {    //try an implicit converter
  def apply(stc:Def#Struct,value:String):Any = null //restore stc.fd.converter.apply(value)     Only makes sense in ObjectMotor 
}
object terminalDegenHandler extends DegenHandler {   //try as for a terminal field
  def apply(stc:Def#Struct,value:String):Any = value
}
object nullDegenHandler extends DegenHandler {       //do nothing
  def apply(stc:Def#Struct,value:String):Any = throw IgnoreDegenException
}
object failDegenHandler extends DegenHandler {       //throw error
  def apply(stc:Def#Struct,value:String):Any = throw new InvalidConversionException(value,null)
}
object includeDegenHandler extends DegenHandler {    //include
  def apply(stc:Def#Struct,value:String):Any = {
    if (!value.startsWith("@include:")) throw new IllegalArgumentException(s"Include expected for $stc: $value")
    //XXX stc.stk.invokeURI(getUri(value))  
  }
  override def isInclude = true
  override def getUri(value:String) = {
    //val u = value.substring(9).split("\\?",2)  //XXX URLEncoder ???
    //val x = URLEncoder.encode(u(0),"UTF-8")+"?"+URLEncoder.encode(u(1),"UTF-8")
    new URI(value.substring(9))
  }
}
