package loader.core

import callbacks._


object Core {

  /** A minimalist root class for Status.
   */
  class Status(val name:String) extends definition.Status

  /** Defines a base structure which simple processors can use, and which can serve as a base for more
   *  complex processors.
   *  @param Ret the intermediate returned type used internaly to convey information from child to parent.
   */
  trait Processor extends definition.Processor {
    type Status  >: Null <: Core.Status
  }

  trait Impl extends definition.Impl with Processor {
    type Element = Elt
    type Status = Core.Status
    /** Motor simplifies the development of processors by focusing on what must be done.
     *  It removes the burden of defining basic classes and builders.
     */
    abstract class Launcher extends super.Launcher {
      //implementations : new top builders (standard ones are in object attach)
      def apply(cbks:Cbks*):Parser=>Element = apply(new Status(""), cbks:_*)
      def apply():Parser=>Element           = apply(new Status(""))
      
      //implementation of a full Element class using the motor defined methods
      class ElementBase(protected var parser0:Parser, val name: String, val parent: Element, val builder: Bld) extends Element with Inner
      class ElementCbks(parser:Parser, name: String, parent: Element, builder: Bld, val cbks: Cbks*)           extends ElementBase(parser,name,parent,builder)         with WithCallbacks
      class ElementCbk (parser:Parser, name: String, parent: Element, builder: Bld, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,name,parent,builder,cbks:_*) with WithCallback {
        override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
      }
      val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld)                      = new ElementBase(parser,c.name,parent,builder)
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld, cbks: Cbks*)         = new ElementCbks(parser,c.name,parent,builder,cbks:_*)
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,c.name,parent,builder,cb,cbks:_*)
      }
    }
  }
}
