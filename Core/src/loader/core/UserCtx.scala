package loader.core

import definition._
import events.EventHandler
import context.Context
import loader.core.context.FieldAnnot
import exceptions._
import loader.core.events.Event


//Note: while type M#BaseParser>:P and M<:P#BaseProcessor are required in the
//      context of the framework, it is of no importance here.
//      actually, constraints here are counter-productive (types UCtx cannot be well defined)
//      of course, contexts built with no regards with these constraint won't be of any real use...
abstract class UsrCtx[-P<:ParserBuilder,-M<:Processor] {
  protected[this] type Proc = M
  protected[this] type Pars = P
  type EltCtx<:EltCtxBase

  /** builds an element context */
  def apply(e:M#Elt):EltCtx
 
  /** this defines every special action that is to be taken for a given Element.
   *  Each element can define its own special actions: these do not have to be defined globally.
   *  However, if you need global actions, the previous method just has to return a constant. 
   */
  protected[this] abstract class EltCtxBase(elt:M#Elt) extends ECtx[P,M](elt) {this:EltCtx=>
    def usrCtx:UsrCtx.this.type = UsrCtx.this
  }
}

abstract class ECtx[P<:ParserBuilder,M<:Processor](elt:M#Elt) {
  /** Solving an include for e with data K */
  def keyMap(s:P#Key):M#Key
  /** Solving an include for e with data K */
  def valMap(s:P#Value):M#Value
  
  /** event handler */
  def eventHandler:EventHandler[M] = null
  /** fast toggle for the parser */
  val fast = true
  /** Solving an 'include' for e with data K */
  def solver(s:M#Value):()=>M#Ret = null
  /** error handler */
  def errHandler(p:P#BaseImpl):PartialFunction[Throwable,Unit] = {
    import p._
    {
      case u:NullPointerException if current==null => try { p.top(StackException(u)) } finally { throw new InternalLoaderException(StackException(u),current) }
      case u:InternalLoaderException => try { current(UnexpectedException(u)) } finally { throw u }
      case u:UserException           => try { current(u) } finally { if (u.lvl<=1) throw u }
      case u:Exception with Event    => try { current(u) } catch { case u:Throwable => throw new InternalLoaderException(u,current) }
      case u:Throwable               => try { current(UnexpectedException(u)) } catch { case _:Throwable => throw new InternalLoaderException(u,current) }
    }
  }
}

