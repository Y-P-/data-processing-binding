package loader.core

import definition._
import events.EventHandler
import context.Context
import loader.core.context.FieldAnnot


//Note: while type M#BaseParser>:P and M<:P#BaseProcessor are required in the
//      context of the framework, it is of no importance here.
//      of course, contexts built with no regards with these constraint
//      won't be of any real use...
abstract class UsrCtx[-P<:ParserBuilder,-M<:Processor] {
  protected[this] type Proc = M
  protected[this] type Pars = P
  def eventHandler:EventHandler[M] = null
  val fast = true

  /** build an element context */
  def apply(e:M#Elt):EltCtx
 
  /** this defines every special action that is to be taken for a given Element.
   *  Each element can define its own special actions: these do not have to be defined globaly.
   *  However, if you need global actions, the previous method just has to return a constant. 
   */
  protected[this] abstract class EltCtx(val elt:M#Elt) extends ECtx[P,M] {
    /** Solving an include for e with data K */
    def solver(s:M#Value):()=>M#Ret = null
    /** Solving an include for e with data K */
    def keyMap(s:P#Key):M#Key
    /** Solving an include for e with data K */
    def valMap(s:P#Value):M#Value
    /** Solving dynamic mappings */
    def solveDynamic(fd:Context#FieldMapping):Context#FieldMapping = null
    /** maps the context in case of an include (different methods may be required) */
   // def map[P1<:ParserBuilder { type BaseProcessor >: M1}, M1<:Processor { type BaseParser >: P1}](q:P1):UsrCtx[P1,M1] = null
  }
}

trait ECtx[P<:ParserBuilder,M<:Processor] {
  def elt:M#Elt
  def solver(s:M#Value):()=>M#Ret
  def keyMap(s:P#Key):M#Key
  def valMap(s:P#Value):M#Value
  def solveDynamic(fd:Context#FieldMapping):Context#FieldMapping
}
