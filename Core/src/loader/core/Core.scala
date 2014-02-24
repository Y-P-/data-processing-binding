package loader.core

import callbacks._

/** A basic complete definition for the common implementation.
 */
object Core {

  /** A minimalist root class for Status.
   */
  class Status[+K>:Null](val key:K) extends definition.Status[K]

  /** Defines a base structure which simple processors can use, and which can serve as a base for more
   *  complex processors.
   *  @param Ret the intermediate returned type used internaly to convey information from child to parent.
   */
  trait Processor extends definition.Processor {
    class Status(val key:Key) extends definition.Status[Key]
  }

  trait Impl extends definition.Impl with Processor {
    /** Motor simplifies the development of processors by focusing on what must be done.
     *  It removes the burden of defining basic classes and builders.
     */
    abstract class Motor extends super.Motor { motor=>
      protected[this] val noStatus = new Core.Status(noKey)
      //implementations : new top builders (standard ones are in object attach)
      def apply(cbks:Cbks*):Parser=>Element = apply(null, cbks:_*)
      def apply():Parser=>Element           = apply(null)
      
      val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld)                      = new Element(parser,motor,c.key,parent,builder)
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld, cbks: Cbks*)         = new ElementCbks(parser,motor,c.key,parent,builder,cbks:_*)
        def apply(parser:Parser, parent: Element, c: Status, builder: Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,motor,c.key,parent,builder,cb,cbks:_*)
      }
    }
    
    // Element implementation : redirect calls
    class Element(protected[this] var parser0:Parser, val motor:Motor, val key: Key, val parent: Element, val builder: Bld)     extends Elt
    protected class ElementCbks(parser:Parser, motor:Motor, key: Key, parent: Element, builder: Bld, val cbks: Cbks*)           extends Element(parser,motor,key,parent,builder)             with WithCallbacks
    protected class ElementCbk (parser:Parser, motor:Motor, key: Key, parent: Element, builder: Bld, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,motor,key,parent,builder,cbks:_*) with WithCallback {
      override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
    }
    
  }
}
