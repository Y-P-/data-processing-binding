package loader.core

import callbacks._

/** A basic complete definition for the common implementation.
 */
trait Core extends definition.Impl {
  type Status = Core.Status[Key]
  protected[this] val noStatus = new Status(noKey)
  type Motor=Launcher
  
  trait Launcher extends super.Launcher {
    def onName(e:Elt,key:Key) = new Status(key)
    //top factories
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](u:U,cbks:Cbks*):X#Impl=>Elt{type Parser=X;type UC=U} = builder(_,u,noStatus,cbks:_*)
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](u:U):X#Impl=>Elt{type Parser=X;type UC=U}            = builder(_,u,noStatus)
  }
  
  def builder(m:Motor) = new Bld {
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, parent: Elt, c: Status)                      = new Element(parser,userCtx,m,c.key,parent)
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, parent: Elt, c: Status, cbks: Cbks*)         = new ElementCbks(parser,userCtx,m,c.key,parent,cbks:_*)
    def apply[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, parent: Elt, c: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,userCtx,m,c.key,parent,cb,cbks:_*)
  }
  
  // Element implementation : redirect calls
  type Element[X<:BaseParser with Singleton,U<:UserCtx[X]] = super.Element[X,U]
  protected class ElementCbks[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, motor:Motor, key: Key, parent: Elt, val cbks: Cbks*) extends Element(parser,userCtx,motor,key,parent) with WithCallbacks
  protected class ElementCbk[X<:BaseParser with Singleton,U<:UserCtx[X]](parser:X#Impl, userCtx:U, motor:Motor, key: Key, parent: Elt, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,userCtx,motor,key,parent,cbks:_*) with WithCallback {
    override def onChild(child:Elt,r:Ret):Unit = super[WithCallback].onChild(child,r)
  }
}
object Core {
  class Status[K>:Null](val key:K) extends definition.Status[K]
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract extends Core
}
