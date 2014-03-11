package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
trait ExtCore extends definition.Impl {
  type Status = ExtCore.Status[Key]
  protected[this] val noStatus = new Status(noKey)
    
  protected[this] type Data  //some data used in association to the element
  def getData(parent:Elt,s:Status):Data
  
  type Motor=Launcher
  trait Launcher extends super.Launcher {
    def onName(e:Elt,key:Key) = new Status(key)
    //top factories
    def apply(cbks:Cbks*):Parser=>Element = apply(noStatus, cbks:_*)
    def apply():Parser=>Element           = apply(noStatus)
  }
  
  def builder(m:Motor) = new Bld {
    def apply(parser:Parser, parent: Element, s: Status)                      = new Elt(parser,m,s,parent)
    def apply(parser:Parser, parent: Element, s: Status, cbks: Cbks*)         = new ElementCbks(parser,m,s,parent, cbks:_*)
    def apply(parser:Parser, parent: Element, s: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,m,s,parent, cb, cbks:_*)
  }
  

  protected class Element[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, motor:Motor, key:Key, parent:Elt, val data:Data) extends super.Element[X,U](parser,userCtx,motor,key,parent)
  protected class ElementCbks[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, motor:Motor, s:Status, parent:Elt, val cbks:Cbks*)         extends Element(parser,userCtx,motor,s.key,parent,getData(parent,s)) with WithCallbacks
  protected class ElementCbk[X<:BaseParser with Singleton,U<:UserCtx[X]] (parser:X#Impl, userCtx:U, motor:Motor, s:Status, parent:Elt, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,userCtx,motor,s,parent,cbks:_*) with WithCallback
}
object ExtCore {
  class Status[K>:Null](key:K) extends Core.Status(key)
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract[+D] extends ExtCore { protected[this] type Data=D }
}