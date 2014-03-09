package loader.core

import callbacks._

/** A basic complete definition for the common implementation.
 */
trait Core extends definition.Impl {
  type Status = Core.Status[Key]
  protected[this] val noStatus = new Status(noKey)
  type Motor[-P<:BaseParser]=Launcher[P]
  
  trait Launcher[-P<:BaseParser] extends super.Launcher[P] {
    def onName(e:Element[_<:P],key:Key) = new Status(key)
    //top factories
    def apply(cbks:Cbks[P]*):Parser[P]=>Element[P] = apply(noStatus, cbks:_*)
    def apply():Parser[P]=>Element[P]           = apply(noStatus)
  }
  
  def builder[P<:BaseParser](m:Motor[P]) = new Bld[P] {
    def apply(parser:Parser[P], parent: Element[P], c: Status)                      = new Element(parser,m,c.key,parent)
    def apply(parser:Parser[P], parent: Element[P], c: Status, cbks: Cbks[P]*)         = new ElementCbks(parser,m,c.key,parent,cbks:_*)
    def apply(parser:Parser[P], parent: Element[P], c: Status, cb:Cbk[P], cbks: Cbks[P]*) = new ElementCbk(parser,m,c.key,parent,cb,cbks:_*)
  }
  
  // Element implementation : redirect calls
  class Element[-P<:BaseParser](parser:Parser[P], motor:Motor[P], key: Key, parent: Element[P])                                                      extends super.EltBase[P](parser,motor,key,parent)
  protected[this] class ElementCbks[-P<:BaseParser](parser:Parser[P], motor:Motor[P], key: Key, parent: Element[P], val cbks: Cbks[P]*)              extends Element(parser,motor,key,parent)             with WithCallbacks[P]
  protected[this] class ElementCbk[-P<:BaseParser] (parser:Parser[P], motor:Motor[P], key: Key, parent: Element[P], val cb: Cbk[P], cbks: Cbks[P]*)  extends ElementCbks(parser,motor,key,parent,cbks:_*) with WithCallback[P] {
    override def onChild(r:Ret):Unit = super[WithCallback].onChild(r)
  }
}
object Core {
  class Status[K>:Null](val key:K) extends definition.Status[K]
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract extends Core
}
