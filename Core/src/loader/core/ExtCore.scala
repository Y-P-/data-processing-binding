package loader.core

/** A more complex implementation for core, where Element is extended through composition with
 *  an arbitrary Data object which can be built from the parent and Status object.
 */
object ExtCore {
  
  trait Def extends Core.Def {
    type Data
    type Element >: Null <: Elt
    
    trait Elt extends super.Elt { this:Element=>
      def data:Data                //a specific associated data class
      def getData(s:Status):Data   //building the data for a given status using this object as parent
    }
  }

  trait Impl extends definition.Impl with Def { self=>
    type Status = Core.Status
    type Element = Elt
      protected def getData(parent:Element,s:Status):Data
      //implementations
      protected def onName(e:Element,name:String) = new Status(name)
      
      class ElementBase(protected var parser0:Parser, val name:String, val parent:Element, val childBuilder:Bld, val data:Data) extends Processor with Element {
        def this(parser:Parser,s:Status,parent:Element,childBuilder:Bld) = this(parser,s.name,parent,childBuilder,getData(parent,s))
        def getData(s:Status) = self.getData(this,s)
      }
      class ElementCbks(parser:Parser, s:Status, parent:Element, childBuilder:Bld, val cbks:Cbks*)         extends ElementBase(parser,s,parent,childBuilder)         with WithCallbacks
      class ElementCbk (parser:Parser, s:Status, parent:Element, childBuilder:Bld, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,s,parent,childBuilder,cbks:_*) with WithCallback

      override val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, s: Status, childBuilder:Bld)                      = new ElementBase(parser,s,parent,childBuilder)
        def apply(parser:Parser, parent: Element, s: Status, childBuilder:Bld, cbks: Cbks*)         = new ElementCbks(parser,s,parent,childBuilder, cbks:_*)
        def apply(parser:Parser, parent: Element, s: Status, childBuilder:Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,s,parent,childBuilder, cb, cbks:_*)
      }
  }
}