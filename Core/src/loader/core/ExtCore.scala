package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
trait ExtCore extends definition.Impl {
  type Status = ExtCore.Status[Key]
  protected[this] val noStatus = new Status(noKey)
    
  type Data  //some data used in association to the element
  def getData(parent:Element,s:Status):Data
  
  type Motor=Launcher
  trait Launcher extends super.Launcher {
    def onName(e:Element,key:Key) = new Status(key)
    //top factories
    def apply(cbks:Cbks*):Parser=>Element = apply(noStatus, cbks:_*)
    def apply():Parser=>Element           = apply(noStatus)
  }
  
  def builder(m:Motor) = new Bld {
    def apply(parser:Parser, parent: Element, s: Status)                      = new Elt(parser,m,s,parent)
    def apply(parser:Parser, parent: Element, s: Status, cbks: Cbks*)         = new ElementCbks(parser,m,s,parent, cbks:_*)
    def apply(parser:Parser, parent: Element, s: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,m,s,parent, cb, cbks:_*)
  }
  

  type Element = Elt
  protected class Elt(parser:Parser, motor:Motor, key:Key, parent:Element, val data:Data) extends super.Elt(parser,motor,key,parent) {
    def this(parser:Parser,motor:Motor,s:Status,parent:Element) = this(parser,motor,s.key,parent,getData(parent,s))
  }
  protected class ElementCbks(parser:Parser, motor:Motor, s:Status, parent:Element, val cbks:Cbks*)         extends Elt(parser,motor,s,parent)                 with WithCallbacks
  protected class ElementCbk (parser:Parser, motor:Motor, s:Status, parent:Element, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,motor,s,parent,cbks:_*) with WithCallback
}
object ExtCore {
  class Status[K>:Null](key:K) extends Core.Status(key)
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract[D] extends ExtCore { type Data=D }
}