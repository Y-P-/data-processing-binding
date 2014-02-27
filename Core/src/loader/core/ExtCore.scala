package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
object ExtCore {
  
  trait Processor extends Core.Processor {
    type Element >: Null <: Elt
    type Data
    def getData(parent:Element,s:Status):Data
        
    trait Elt extends super.Elt { this:Element=>
      def data:Data                //a specific associated data class
    }
    
  }
  
  trait Impl extends Core.Impl with Processor {
    
    abstract class Motor extends super.Motor { motor=>
      def onName(e:Element,key:Key) = new Status(key)
      override val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, s: Status)                      = new Element(parser,motor,s,parent)
        def apply(parser:Parser, parent: Element, s: Status, cbks: Cbks*)         = new ElementCbks(parser,motor,s,parent, cbks:_*)
        def apply(parser:Parser, parent: Element, s: Status, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,motor,s,parent, cb, cbks:_*)
      }
    }

    class Element(parser0:Parser, motor:Motor, key:Key, parent:Element, val data:Data) extends super.Element(parser0,motor,key,parent) with Elt {
      def this(parser:Parser,motor:Motor,s:Status,parent:Element) = this(parser,motor,s.key,parent,getData(parent,s))
    }
    protected class ElementCbks(parser:Parser, motor:Motor, s:Status, parent:Element, val cbks:Cbks*)         extends Element(parser,motor,s,parent)             with WithCallbacks
    protected class ElementCbk (parser:Parser, motor:Motor, s:Status, parent:Element, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,motor,s,parent,cbks:_*) with WithCallback
  }
}