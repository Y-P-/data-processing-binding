package parser.px.analyzers.predefs

import parser.Source
import parser.px.Analyzer
import parser.px.analyzers.SimpleAnalyzer

/**
 * A simple analyzer used to just parse the data and see whether it complies to the PX grammar.
 */
protected class ParseAnalyzer extends Analyzer with SimpleAnalyzer {
  protected type R = Unit
  protected def ignore(name:Tk):Boolean  = false
  protected def closeList()              = { }
  protected def closeStruct()            = { }
  protected def closeArray()             = { }
  protected def appendList(value:Tk)     = { }
  protected def putStruct(name:Tk)       = { }
  protected def putAnonStruct()          = { }
  protected def putField(name:Tk,data:Tk)= { }
  protected def putList(name:Tk)         = { }
  protected def putEmpty(name:Tk)        = { }
  protected def putArray(name:Tk)        = { }
  protected val end = ()
}

object ParseAnalyzer extends ParseAnalyzer