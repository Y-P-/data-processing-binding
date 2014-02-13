package loader.core

import definition._
import events.EventHandler
import context.Context
import loader.core.names.QName
import loader.core.context.FieldAnnot


abstract class UserContext[-E<:Def#Elt] {
  protected[this] type Elt = E
  //the handler for generated events 
  def eventHandler:EventHandler[_>:E] = null
  //parameter that asks the parser to accelerate where it can (i.e. skip unecessary data)
  val fast = true
  /** build an element context */
  def apply(e:Elt):EltContext
  
  class EltContext(protected[this] val elt:Elt) {
    protected[this] type K = elt.Kind
    /** qName for the element */
    def qName = QName(elt)
    /** Solving an include for e with data K */
    def solver(s:K):()=>elt.Ret = null
    /** Solving dynamic mappings */
    def solveDynamic(fd:Context#FieldMapping):Context#FieldMapping = null
  }
}
object UserContext {
  type Ctx[-k,-E<:Def#Elt { type Kind <: k }] = UserContext[E]
}


