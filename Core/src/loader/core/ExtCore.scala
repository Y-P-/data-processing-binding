package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
trait ExtCore extends definition.Impl {
  type Status = ExtCore.Status[Key]
  type Dlg=DlgBase
  type Elt = EltBase
  protected[this] val noStatus = new Status(noKey)
  
  protected[this] type Data>:Null    //some data used in association to the element
  def getData(parent:Elt,s:Status):Data

  trait EltBase extends super.EltBase {
    def data: Data
  }
  
  trait DlgBase extends super.DlgBase {
    def onName(e:Elt,key:Key) = new Status(key)
  }
    
  def builder(dlg:Dlg) = new EltBuilder {
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, s: Status):Element[X]                                        = new Element(parser,userCtx,dlg,s.key,parent,getData(parent,s))
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, s: Status, cbks: Cbks*):Element[X] with WithCallbacks        = new ElementCbks(parser,userCtx,dlg,s,parent, cbks:_*)
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, s: Status, cb:Cbk, cbks: Cbks*):Element[X] with WithCallback = new ElementCbk(parser,userCtx,dlg,s,parent, cb, cbks:_*)
  }
  
  def apply[X<:BaseParser with Singleton](u:UCtx[X],dlg:Dlg)           :X#Parser=>Element[X] = dlg.builder(_,u,null,noStatus)
  def apply[X<:BaseParser with Singleton](u:UCtx[X],dlg:Dlg,cbks:Cbks*):X#Parser=>Element[X] = dlg.builder(_,u,null,noStatus,cbks:_*)
  
  protected class Element    [X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, key:Key,  parent:Elt, val data:Data)          extends ElementBase[X](parser,userCtx,dlg,key,parent) with EltBase
  protected class ElementCbks[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, s:Status, parent:Elt, val cbks:Cbks*)         extends Element(parser,userCtx,dlg,s.key,parent,getData(parent,s)) with WithCallbacks
  protected class ElementCbk [X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, s:Status, parent:Elt, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback
}
object ExtCore {
  class Status[K>:Null](key:K) extends Core.Status(key)
  //using Abstract prevents code bloating due to trait expansion
  abstract class Abstract[+D>:Null] extends ExtCore { protected[this] type Data=D }
}