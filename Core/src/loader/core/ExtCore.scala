package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
object ExtCore {
  
  trait Processor extends Core.Processor {
    type Data
    type Element >: Null <: Elt
    
    trait Elt extends super.Elt { this:Element=>
      def data:Data                //a specific associated data class
      def getData(s:Status):Data   //building the data for a given status using this object as parent
    }
  }

  trait Impl extends definition.Impl with Processor { self=>
    type Status = Core.Status
    type Element = Elt
    
    abstract class Launcher extends super.Launcher {
      protected def getData(parent:Element,s:Status):Data
      //implementations
      protected def onName(e:Element,name:String) = new Status(name)
      
      class ElementBase(protected var parser0:Parser, val name:String, val parent:Element, val builder:Bld, val data:Data) extends Inner with Elt {
        def this(parser:Parser,s:Status,parent:Element,builder:Bld) = this(parser,s.name,parent,builder,getData(parent,s))
        def getData(s:Status) = Launcher.this.getData(this,s)
      }
      class ElementCbks(parser:Parser, s:Status, parent:Element, builder:Bld, val cbks:Cbks*)         extends ElementBase(parser,s,parent,builder)         with WithCallbacks
      class ElementCbk (parser:Parser, s:Status, parent:Element, builder:Bld, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,s,parent,builder,cbks:_*) with WithCallback
    
      override val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, s: Status, builder:Bld)                      = new ElementBase(parser,s,parent,builder)
        def apply(parser:Parser, parent: Element, s: Status, builder:Bld, cbks: Cbks*)         = new ElementCbks(parser,s,parent,builder, cbks:_*)
        def apply(parser:Parser, parent: Element, s: Status, builder:Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,s,parent,builder, cb, cbks:_*)
      }
    }
  }
}