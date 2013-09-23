package parser.px.analyzers.predefs
import parser.px.Analyzer
import parser.px.analyzers.{NoFilter,StructAnalyzer}
import parser.Source
import parser.px.analyzers.structured.predefs.CompactField

object CompactStructuredAnalyzer extends Analyzer with StructAnalyzer with CompactField with NoFilter
