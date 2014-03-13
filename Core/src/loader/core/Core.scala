package loader.core

import callbacks._

/** A basic complete definition for the common implementation.
 */
trait Core extends definition.Impl {
  type Status = Core.Status[Key]
  type Dlg <: DlgBase
  type Elt = EltBase
  protected[this] val noStatus = new Status(noKey)
  
  trait DlgBase extends super.DlgBase {this:Dlg=>
    def onName(e:Elt,key:Key) = new Status(key)
    def apply[X<:BaseParser with Singleton](u:UCtx[X],cbks:Cbks*): X#Parser=>Element[X]  = builder(_,u,null,noStatus,cbks:_*)
    def apply[X<:BaseParser with Singleton](cbks:Cbks*): UCtx[X] => X#Parser=>Element[X] = apply(_,cbks:_*)
  }
  
  def builder(d:Dlg) = new EltBuilder {
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, c: Status):Element[X]                                        = new Element(parser,userCtx,d,c.key,parent)
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, c: Status, cbks: Cbks*):Element[X] with WithCallbacks        = new ElementCbks(parser,userCtx,d,c.key,parent,cbks:_*)
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, c: Status, cb:Cbk, cbks: Cbks*):Element[X] with WithCallback = new ElementCbk(parser,userCtx,d,c.key,parent,cb,cbks:_*)
  }
  
  def apply[X<:BaseParser with Singleton](u:UCtx[X],dlg:Dlg)           :X#Parser=>Element[X] = dlg.builder(_,u,null,noStatus)
  def apply[X<:BaseParser with Singleton](u:UCtx[X],dlg:Dlg,cbks:Cbks*):X#Parser=>Element[X] = dlg.builder(_,u,null,noStatus,cbks:_*)
  
  // Element implementation : redirect calls
  protected class Element    [X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, key:Key, parent:Elt) extends ElementBase[X](parser,userCtx,dlg,key,parent) with Elt
  protected class ElementCbks[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, key:Key, parent: Elt, val cbks: Cbks*) extends Element(parser,userCtx,dlg,key,parent) with WithCallbacks
  protected class ElementCbk [X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, key:Key, parent: Elt, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,userCtx,dlg,key,parent,cbks:_*) with WithCallback {
    override def onChild(child:Elt,r:Ret):Unit = super[WithCallback].onChild(child,r)
  }
}
object Core {
  class Status[K>:Null](val key:K) extends definition.Status[K]
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract extends Core
}
