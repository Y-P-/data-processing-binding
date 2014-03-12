package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
trait ExtCore extends definition.Impl {
  type Status = ExtCore.Status[Key]
  type Motor=Launcher
  type Elt = EltBase
  protected[this] val noStatus = new Status(noKey)
  
  protected[this] type Data  //some data used in association to the element
  def getData(parent:Elt,s:Status):Data

  trait EltBase extends super.EltBase {
    def data: Data
  }
  
  trait Launcher extends super.Launcher {
    def onName(e:Elt,key:Key) = new Status(key)
    //top factories
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](u:U,cbks:Cbks*):X#Impl=>Elt{type Parser=X;type UC=U} = builder(_,u,noStatus,cbks:_*)
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](u:U):X#Impl=>Elt{type Parser=X;type UC=U}            = builder(_,u,noStatus)
  }
    
  def builder(m:Motor) = null/*new EltBuilder {
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, parent: Elt, s: Status)                      = new Element(parser,userCtx,m,s.key,parent,getData(parent,s))
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, parent: Elt, s: Status, cbks: Cbks*)         = new ElementCbks(parser,userCtx,m,s,parent, cbks:_*)
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, parent: Elt, s: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,userCtx,m,s,parent, cb, cbks:_*)
  }*/
  
  protected class Element[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, motor:Motor, key:Key, parent:Elt, val data:Data)               extends super.Element[X,U](parser,userCtx,motor,key,parent) with EltBase
  protected class ElementCbks[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, motor:Motor, s:Status, parent:Elt, val cbks:Cbks*)         extends Element(parser,userCtx,motor,s.key,parent,getData(parent,s)) with WithCallbacks
  protected class ElementCbk[X<:BaseParser with Singleton,U<:UserCtx[X]] (parser:X#Impl, userCtx:U, motor:Motor, s:Status, parent:Elt, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,userCtx,motor,s,parent,cbks:_*) with WithCallback
}
object ExtCore {
  class Status[K>:Null](key:K) extends Core.Status(key)
  //using Abstract prevents code bloating due to trait expansion
  abstract class Abstract[+D] extends ExtCore { protected[this] type Data=D }
}