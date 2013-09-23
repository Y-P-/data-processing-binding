package parser.px.analyzers.predefs
import parser.px.Analyzer
import parser.px.analyzers.{NoFilter,StructAnalyzer}
import parser.px.analyzers.structured.predefs.SimpleField

object SimpleStructuredAnalyzer extends Analyzer with StructAnalyzer with SimpleField with NoFilter
