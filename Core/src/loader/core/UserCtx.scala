package loader.core

import definition._
import events.EventHandler
import context.Context
import loader.core.names.QName
import loader.core.context.FieldAnnot


abstract class UserContext[-P<:Processor] {
  protected[this] type Proc = P
  protected[this] type Elt = P#Element
  //the handler for generated events 
  def eventHandler:EventHandler[P] = null
  //parameter that asks the parser to accelerate where it can (i.e. skip unecessary data)
  val fast = true
  /** build an element context */
  def apply(e:Elt):EltContext
  
  protected[this] class EltContext(protected[this] val elt:Elt) {
    /** qName for the element */
    def qName = QName(elt)
    /** Solving an include for e with data K */
    def solver(s:P#Kind):()=>P#Ret = null
    /** Solving dynamic mappings */
    def solveDynamic(fd:Context#FieldMapping):Context#FieldMapping = null
  }
}
object UserContext {
  type Ctx[-k,-P<:Processor { type Kind <: k }] = UserContext[P]
}


