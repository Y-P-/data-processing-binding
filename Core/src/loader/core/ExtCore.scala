package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
trait ExtCore extends definition.Impl {
  type Status = ExtCore.Status[Key]
  protected[this] val noStatus = new Status(noKey)
    
  protected[this] type Data  //some data used in association to the element
  def getData[P<:BaseParser](parent:Element[P],s:Status):Data
  
  type Motor[-P<:BaseParser]=Launcher[P]
  trait Launcher[-P<:BaseParser] extends super.Launcher[P] {
    def onName(e:Element[_<:P],key:Key) = new Status(key)
    //top factories
    def apply(cbks:Cbks[P]*):Parser[P]=>Element[P] = apply(noStatus, cbks:_*)
    def apply():Parser[P]=>Element[P]           = apply(noStatus)
  }
  
  def builder[P<:BaseParser](m:Motor[P]) = new Bld[P] {
    def apply(parser:Parser[P], parent: Element[P], s: Status)                            = new Element(parser,m,s,parent)
    def apply(parser:Parser[P], parent: Element[P], s: Status, cbks: Cbks[P]*)            = new ElementCbks(parser,m,s,parent, cbks:_*)
    def apply(parser:Parser[P], parent: Element[P], s: Status, cb:Cbk[P], cbks: Cbks[P]*) = new ElementCbk(parser,m,s,parent, cb, cbks:_*)
  }
  

  class Element[-P<:BaseParser](parser:Parser[P], motor:Motor[P], key:Key, parent:Element[P], val data:Data) extends super.EltBase[P](parser,motor,key,parent) {
    def this(parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P]) = this(parser,motor,s.key,parent,getData(parent,s))
  }
  protected[this] class ElementCbks[P<:BaseParser](parser:Parser[P], motor:Motor[P], s:Status, parent:Element[P], val cbks:Cbks[P]*)            extends Element(parser,motor,s,parent)             with WithCallbacks[P]
  protected[this] class ElementCbk[P<:BaseParser] (parser:Parser[P], motor:Motor[P], s:Status, parent:Element[P], val cb:Cbk[P], cbks:Cbks[P]*) extends ElementCbks(parser,motor,s,parent,cbks:_*) with WithCallback[P]
}
object ExtCore {
  class Status[K>:Null](key:K) extends Core.Status(key)
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract[+D] extends ExtCore { protected[this] type Data=D }
}