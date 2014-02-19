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
  trait Def extends definition.Def {
    type Status  >: Null <: Core.Status
  }

  trait Impl extends definition.Impl with Def {
    type Element = Elt
    type Status = Core.Status
    /** Motor simplifies the development of processors by focusing on what must be done.
     *  It removes the burden of defining basic classes and builders.
     */
    abstract class Launcher[+BP<:BaseParser with Singleton](bp:BP) extends super.Launcher[BP](bp) {
      //implementations : new top builders (standard ones are in object attach)
      def apply(cbks:Cbks*):X = apply(new Status(""), cbks:_*)
      def apply():X           = apply(new Status(""))
      
      //implementation of a full Element class using the motor defined methods
      class ElementBase(protected var parser0:Parser, val name: String, val parent: Element, val childBuilder: Bld) extends Element with Processor
      class ElementCbks(parser:Parser, name: String, parent: Element, childBuilder: Bld, val cbks: Cbks*)          extends ElementBase(parser,name,parent,childBuilder)         with WithCallbacks
      class ElementCbk (parser:Parser, name: String, parent: Element, childBuilder: Bld, val cb: Cbk, cbks: Cbks*) extends ElementCbks(parser,name,parent,childBuilder,cbks:_*) with WithCallback {
        override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
      }
      val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, c: Status, childBuilder: Bld)                      = new ElementBase(parser,c.name,parent,childBuilder)
        def apply(parser:Parser, parent: Element, c: Status, childBuilder: Bld, cbks: Cbks*)         = new ElementCbks(parser,c.name,parent,childBuilder,cbks:_*)
        def apply(parser:Parser, parent: Element, c: Status, childBuilder: Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,c.name,parent,childBuilder,cb,cbks:_*)
      }
    }
  }
}
