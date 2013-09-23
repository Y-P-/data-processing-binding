package loader.features

import loader.exceptions.InvalidConversionException
import loader.core.CtxCore.Def

abstract class DegenListHandler {
  /**
   * Handles a degenerated form of a list (as a String)
   * @param lst, the degenerated list
   * @param value, it's degenerated String form
   * @return the values for the degen list, which is assumed to be composed of simple tokens (a list of struts cannot degen at this time)
   */
  def apply(lst:Def#List,value:String):Array[String]
}

object failDegenListHandler extends DegenListHandler {     //throw error
  def apply(lst:Def#List,value:String):Array[String] = throw new InvalidConversionException("value",null)
}
object nullListDegenHandler extends DegenListHandler {     //do nothing
  def apply(lst:Def#List,value:String):Array[String] = throw IgnoreDegenException
}

protected class BaseCsvDegenListHandler extends DegenListHandler {     //split string
  import BaseCsvDegenListHandler._
  //the separator is either inside the initial brackets, as in "[;]aze;uio;gffg"
  //or inferred from a limited list of separators: <,;:/\ >
  //in the latter case, the first seen in the text is assumed to be the used separator
  def apply(lst:Def#List,value:String):Array[String] = {
    val m1 = p1.matcher(value)
    if (m1.matches)                             m1.group(2).split(m1.group(1).charAt(1))
    else {
      val m2 = p2.matcher(value)
      if (m2.matches && m2.group(2).length>0)   value.split(m2.group(2).charAt(0))
      else                                      Array(value) //if not found, only one token assume : any separator goes
    }
  }
}
protected object BaseCsvDegenListHandler {
  import java.util.regex.Pattern  
  protected val p1 = Pattern.compile("(\\[.\\])(.*)")
  protected val p2 = Pattern.compile("(.+?)([,;:\\\\/\\s-]).*")
}

object csvDegenListHandler extends BaseCsvDegenListHandler

object csvTrimmedDegenListHandler extends BaseCsvDegenListHandler {
  override def apply(lst:Def#List,value:String):Array[String] = super.apply(lst,value).map(_.trim)
}
