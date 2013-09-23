package loader.core

import loader.Context
import loader.core.callbacks.Callback
import loader.core.definition._
import loader.core.events.EventHandler

class UserContext[-Elt<:Def#Elt] {
  type E = Elt
  //the handler for generated events 
  def eventHandler:EventHandler[E] = null
  //parameter that asks the parser to accelerate where it can (i.e. skip unecessary data)
  val fast = true
  //parameters for substitution
  val vars:scala.collection.Map[String,String] = null
  //dynamic solver
  def solveDynamic(elt:E,fd:Context#FieldMapping):Context#FieldMapping = null
  
  /** For includes */
  //detects includes: called only in onVal calls.
  def isInclude(elt:E)(s:elt.Kind):Boolean = false
  //include solver
  def solveInclude(elt:E)(s:elt.Kind):ParserBuilder#Executor = new loader.parsers.Struct(false).run(new java.io.File("verysmall1.txt").toURI, "UTF-8")
  //mapper from x.Kind to elt.Kind
  def getMapper(elt:E)(x:ParserBuilder#Executor):(elt.Element,x.builder.Kind)=>elt.Kind = null
}
