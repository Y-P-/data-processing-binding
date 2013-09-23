package loader.core

import definition._
import callbacks.Callback
import events.EventHandler
import context.Context
import names.QualifiedName

abstract class UserContext[-Elt<:Def#Elt] {
  type E = Elt
  //the handler for generated events 
  def eventHandler:EventHandler[E] = null
  //parameter that asks the parser to accelerate where it can (i.e. skip unecessary data)
  val fast = true
  //for name conversions
  def outName(elt:E):String = elt.name
  //parameters for substitution
  val vars:scala.collection.Map[String,String] = null
  //dynamic solver
  def solveDynamic(elt:E,fd:Context#FieldMapping):Context#FieldMapping = null
  //Qualified name builder
  def qname(elt:E):QualifiedName.Scheme
  //Qualified name builder
  def nameConverter(elt:E):QualifiedName.Converter
  
  /** For includes */
  //detects includes: called only in onVal calls.
  def isInclude(elt:E)(s:elt.Kind):Boolean = false
  //include solver
  def solveInclude(elt:E)(s:elt.Kind):ParserBuilder#Executor = null
  //mapper from x.Kind to elt.Kind
  def getMapper(elt:E)(x:ParserBuilder#Executor):(elt.Element,x.builder.Kind)=>elt.Kind = null
}
