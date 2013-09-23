package parser.px.analyzers

import parser.px.Analyzer
import parser.Source

/**
 * The simple analyzer is for fast and simple work.
 * On abort, the standard work is done on closing aborted elements.
 * Position indexes are ignored.
 */
trait SimpleAnalyzer extends Analyzer {

  protected def closeList():Unit
  protected def closeStruct():Unit
  protected def closeArray():Unit
  protected def putStruct(name:Tk):Unit
  protected def putAnonStruct():Unit
  protected def putList(name:Tk):Unit
  protected def putEmpty(name:Tk):Unit
  protected def putArray(name:Tk):Unit
  
  protected final def closeList(close:Int):Unit                = closeList
  protected final def closeStruct(close:Int):Unit              = closeStruct
  protected final def closeArray(close:Int):Unit               = closeArray
  protected final def putStruct(name:Tk,open:Int):Unit         = putStruct(name)
  protected final def putList(name:Tk,open:Int):Unit           = putList(name)
  protected final def putEmpty(name:Tk,open:Int,close:Int):Unit= putEmpty(name)
  protected final def putArray(name:Tk,open:Int):Unit          = putArray(name)
  protected final def putAnonStruct(open:Int):Unit             = putAnonStruct
  protected final def abortList(close:Int):Unit                = closeList
  protected final def abortStruct(close:Int):Unit              = closeStruct
  protected final def abortArray(close:Int):Unit               = closeArray

  def apply(src:Source) = super.apply(src,0)
}