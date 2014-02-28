package loader.core

import callbacks._

/** A basic complete definition for the common implementation.
 */
trait Core extends definition.Impl {
  type Status = Core.Status[Key]
  protected[this] val noStatus = new Status(noKey)
  
  abstract class Motor extends super.Motor { motor=>
    def onName(e:Element,key:Key) = new Status(key)
    //top factories
    def apply(cbks:Cbks*):Parser=>Element = apply(noStatus, cbks:_*)
    def apply():Parser=>Element           = apply(noStatus)
    
    protected def getBld:Bld = new Bld {
      def apply(parser:Parser, parent: Element, c: Status)                      = new Elt(parser,motor,c.key,parent)
      def apply(parser:Parser, parent: Element, c: Status, cbks: Cbks*)         = new ElementCbks(parser,motor,c.key,parent,cbks:_*)
      def apply(parser:Parser, parent: Element, c: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,motor,c.key,parent,cb,cbks:_*)
    }
  }
    
  type Element = Elt
  // Element implementation : redirect calls
  protected class Elt(parser:Parser, motor:Motor, key: Key, parent: Element)                                    extends super.Elt(parser,motor,key,parent)
  protected class ElementCbks(parser:Parser, motor:Motor, key: Key, parent: Element, val cbks: Cbks*)           extends Elt(parser,motor,key,parent)                 with WithCallbacks
  protected class ElementCbk (parser:Parser, motor:Motor, key: Key, parent: Element, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,motor,key,parent,cbks:_*) with WithCallback {
    override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
  }
}
object Core {
  class Status[K>:Null](val key:K) extends definition.Status[K]
  abstract class Abstract extends Core
}
