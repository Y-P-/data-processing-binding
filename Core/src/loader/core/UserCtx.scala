package loader.core

import definition._
import events.EventHandler
import context.Context
import loader.core.names.QName

abstract class UserContext[-Elt<:Def#Elt] {
  protected[this] type E = Elt
  //the handler for generated events 
  def eventHandler:EventHandler[E] = null
  //parameter that asks the parser to accelerate where it can (i.e. skip unecessary data)
  val fast = true
  //for name conversions
  def qName(elt:E):QName = QName(elt)
  //for local name fast access when this applies. Override this if neither parser not processor use namespaces/attributes
  def localName(elt:E):String = qName(elt).local
  //XXX to be removed parameters for substitution
  val vars:scala.collection.Map[String,String] = null
  //dynamic solver
  def solveDynamic(elt:E,fd:Context#FieldMapping):Context#FieldMapping = null
  
  /** For includes */
  //detects includes: called only in onVal calls.
  def isInclude(elt:E)(s:elt.Kind):Boolean = false
  //include solver
  def solveInclude(elt:E)(s:elt.Kind):ParserBuilder#Executor = null
  //mapper from x.Kind to elt.Kind
  def getMapper(elt:E)(x:ParserBuilder#Executor):(elt.Element,x.builder.Kind)=>elt.Kind = null
}
