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
      def onName(e:Element,key:Key) = new Status(key)
      protected[this] val noStatus = new Status(noKey)
      //implementations : new top builders (standard ones are in object attach)
      def apply(cbks:Cbks*):Parser=>Element = apply(noStatus, cbks:_*)
      def apply():Parser=>Element           = apply(noStatus)
      
      val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, c: Status)                      = new Element(parser,motor,c.key,parent)
        def apply(parser:Parser, parent: Element, c: Status, cbks: Cbks*)         = new ElementCbks(parser,motor,c.key,parent,cbks:_*)
        def apply(parser:Parser, parent: Element, c: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,motor,c.key,parent,cb,cbks:_*)
      }
    }
    
    // Element implementation : redirect calls
    class Element(protected[this] var parser0:Parser, val motor:Motor, val key: Key, val parent: Element)         extends super[Impl].Elt
    protected class ElementCbks(parser:Parser, motor:Motor, key: Key, parent: Element, val cbks: Cbks*)           extends Element(parser,motor,key,parent)             with WithCallbacks
    protected class ElementCbk (parser:Parser, motor:Motor, key: Key, parent: Element, val cb: Cbk, cbks: Cbks*)  extends ElementCbks(parser,motor,key,parent,cbks:_*) with WithCallback {
      override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
    }
    
  }
}
