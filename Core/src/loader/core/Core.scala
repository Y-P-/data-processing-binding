package loader.core

import callbacks._


object Core {

  /** A minimalist root class for Status.
   */
  class Status[+K>:Null](val key:K) extends definition.Status[K]

  /** Defines a base structure which simple processors can use, and which can serve as a base for more
   *  complex processors.
   *  @param Ret the intermediate returned type used internaly to convey information from child to parent.
   */
  trait Processor extends definition.Processor {
    type Status  >: Null <: Core.Status[Key]
  }

  trait Impl extends definition.Impl with Processor {
    type Element = Elt
    type Status = Core.Status[Key]
    /** Motor simplifies the development of processors by focusing on what must be done.
     *  It removes the burden of defining basic classes and builders.
     */
    abstract class Launcher extends super.Launcher {
      protected[this] val noStatus = new Core.Status(noKey)
      //implementations : new top builders (standard ones are in object attach)
      def apply(cbks:Cbks*):Parser=>Element = apply(noStatus, cbks:_*)
      def apply():Parser=>Element           = apply(noStatus)
      
      //implementation of a full Element class using the motor defined methods
      class ElementBase(protected var parser0:Parser, val key: Key, val parent: Element, val builder: Bld) extends Element with Inner
      class ElementCbks(parser:Parser, key: Key, parent: Element, builder: Bld, val cbks: Cbks*)           extends ElementBase(parser,key,parent,builder)         with WithCallbacks
      class ElementCbk (parser:Parser, key: Key, parent: Element, builder: Bld, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,key,parent,builder,cbks:_*) with WithCallback {
        override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
      }
      val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld)                      = new ElementBase(parser,c.key,parent,builder)
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld, cbks: Cbks*)         = new ElementCbks(parser,c.key,parent,builder,cbks:_*)
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,c.key,parent,builder,cb,cbks:_*)
      }
    }
  }
}
