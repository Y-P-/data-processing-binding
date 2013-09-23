package parser.px.analyzers.predefs

import parser.Source
import parser.px.Analyzer
import parser.px.analyzers.SimpleAnalyzer

/**
 * A simple analyzer used to just parse the data and see whether it complies to the PX grammar.
 * It echoes what it parses to the output as a one line file.
 */
protected class PrintAnalyzer extends Analyzer with SimpleAnalyzer {
  protected type R = Unit
  protected def tab() = print(" ")
  protected def ignore(name:Tk):Boolean  = false
  protected def closeList()              = { print("}"); }
  protected def closeStruct()            = { tab; print("}") }
  protected def closeArray()             = { tab; print("}") }
  protected def appendList(value:Tk)     = { print(value.baseString+" ") }
  protected def putStruct(name:Tk)       = { tab; if (name==null) print("{") else print(name.baseString+"= {") }
  protected def putAnonStruct()          = { tab; print("{") }
  protected def putField(name:Tk,data:Tk)= { tab; print(name.baseString+"="+data.baseString) }
  protected def putList(name:Tk)         = { tab; print(name.baseString+"= {") }
  protected def putEmpty(name:Tk)        = { tab; print(name.baseString+"= {}") }
  protected def putArray(name:Tk)        = { tab; print(name.baseString+"= {") }
  protected val end                      = ()
}

object PrettyPrintAnalyzer extends PrintAnalyzer {
  override def tab() = { println; val sb= new StringBuffer; for (i <- 0 until depth) sb.append("  "); print(sb) }
}

object PrintAnalyzer extends PrintAnalyzer