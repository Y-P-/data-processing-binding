package loader.core

abstract class ParserSpawner {
  type Parser <: ParserBuilder
  def apply(pr: utils.ParamReader):Parser
  def apply(params:scala.collection.Map[String,String]):Parser = apply(new utils.ParamReader(params))
}