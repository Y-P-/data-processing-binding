package loader.core

import definition._
import events.EventHandler
//import context.Context
//import loader.core.context.FieldAnnot


abstract class UserContext[-P<:ParserBuilder,-M<:Processor] {
  protected[this] type Proc = M
  protected[this] type Elt  = M#Element[_<:P]
  //the handler for generated events 
  def eventHandler:EventHandler[M] = null
  //parameter that asks the parser to accelerate where it can (i.e. skip unecessary data)
  val fast = true
  /** build an element context */
  def apply(e:Elt):EltContext
  
  protected[this] class EltContext(protected[this] val elt:Elt) {
    /** Solving an include for e with data K */
    def solver(s:M#Value):()=>M#Ret = null
    /** Solving dynamic mappings */
  //  def solveDynamic(fd:Context#FieldMapping):Context#FieldMapping = null
    /** Converting Keys from parser to Procesor ; by default, dirty cast (types assumed equal) */
    def keyMap(s:P#Key):M#Key = s.asInstanceOf[M#Key]
    /** Converting Keys from parser to Procesor ; by default, dirty cast (types assumed equal) */
    def valMap(s:P#Value):M#Value = s.asInstanceOf[M#Value]
    /** maps the context in case of an include (different methods may be required) */
   // def map[P1<:ParserBuilder { type BaseProcessor >: M1}, M1<:Processor { type BaseParser >: P1}](q:P1):UsrCtx[P1,M1] = null
  }
}
