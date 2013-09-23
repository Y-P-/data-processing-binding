package loader.core.test

import loader.core._
import loader.core.callbacks._

object o extends Core.Impl {
  type Kind = String
  type Ret  = Unit
  type Parser = ParserBuilder#Impl
  def parsClass = classOf[Parser]
  def kindClass = classOf[Kind]

  def apply(pr: utils.ParamReader,userCtx:loader.core.UserContext[loader.core.test.o.Element]):Impl = throw new IllegalStateException
}

class Print extends o.Motor {
  val userCtx:UserContext[Element] = null
  type Element = o.Element
  type Result = Unit

  protected def onName(e:Element,name: String):Core.Status = { println("creating "+name); new Core.Status(name) }
  protected def onBeg(e:Element): Unit = println(e.name+" = {")
  protected def onVal(e:Element,v: String): o.Ret = println("   -> "+v)
  protected def onEnd(e:Element): o.Ret = println("}")
  protected def onChild(e:Element,child: Element, r: o.Ret): Unit = println(e.name+" receiving "+child.name)

  protected def onInit():Unit = println("start")
  protected def onExit():Unit = println("finished")
}

class Print1 extends Print {
  override protected def onBeg(e:Element): Unit = println(e.name+" = ||{") 
}

class Print2 extends Print1 {
  import o._
  class Elt0(parser:Parser, name:String, parent:Element, childBuilder:Bld) extends super.ElementBase(parser,name,parent,childBuilder) with Processor {
    override protected def onBeg(): Unit = println(name+" = |{") 
  }
  class ElementCbks(parser:Parser,name:String, parent:Element, childBuilder:Bld, val cbks:Cbks*)         extends Elt0(parser,name,parent,childBuilder)                with WithCallbacks
  class ElementCbk (parser:Parser,name:String, parent:Element, childBuilder:Bld, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,name,parent,childBuilder,cbks:_*) with WithCallback {
      override def onChild(child:Element,r:Ret):Unit = super[WithCallback].onChild(child,r)
    }
  override val builder:Bld = new Bld {
    def apply(parser:Parser, parent: Element, c: Status, childBuilder:Bld)                      = new Elt0(parser,c.name,parent,childBuilder)
    def apply(parser:Parser, parent: Element, c: Status, childBuilder:Bld, cbks: Cbks*)         = new ElementCbks(parser,c.name,parent,childBuilder,cbks:_*)
    def apply(parser:Parser, parent: Element, c: Status, childBuilder:Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,c.name,parent,childBuilder,cb,cbks:_*)
  }
}

class Print3 extends o.Motor {
  val userCtx:UserContext[o.Element] = null
  import o._
  type Result = Unit
  protected def onName(e:Element,name: String):Core.Status = null
  protected def onBeg(e:Element): Unit = ()
  protected def onVal(e:Element,v: String): o.Ret = ()
  protected def onEnd(e:Element): o.Ret = ()
  protected def onChild(e:Element,child: Element, r: o.Ret): Unit = ()

  protected def onInit():Unit = ()
  protected def onExit():Unit = ()
  
  class Elt0(val parser:Parser, val name:String, val parent:Element, val childBuilder:Bld) extends o.Element {
    protected def parser_=(parser:Parser):Unit = ()
    def userCtx = Print3.this.userCtx
    protected def onName(name: String): Status = { println("creating "+name); new Core.Status(name) }
    protected def onBeg(): Unit = println(name+" = (")
    protected def onVal(v: String): Ret = println("   => "+v)
    protected def onEnd(): Ret = println(")")
    protected def onChild(child: Element, r: Ret): Unit = println(name+" receiving "+child.name)
  }
  class ElementCbks(parser:Parser, name:String, parent:Element, childBuilder:Bld, val cbks:Cbks*)         extends Elt0(parser,name,parent,childBuilder)             with WithCallbacks
  class ElementCbk (parser:Parser, name:String, parent:Element, childBuilder:Bld, val cb:Cbk, cbks:Cbks*) extends ElementCbks(parser,name,parent,childBuilder,cbks:_*) with WithCallback
  override val builder:Bld = new Bld {
    def apply(parser:Parser, parent: Element, c: Status, childBuilder:Bld)                      = new Elt0(parser,c.name,parent,childBuilder)
    def apply(parser:Parser, parent: Element, c: Status, childBuilder:Bld, cbks: Cbks*)         = new ElementCbks(parser,c.name,parent,childBuilder,cbks:_*)
    def apply(parser:Parser, parent: Element, c: Status, childBuilder:Bld, cb:Cbk, cbks: Cbks*) = new ElementCbk(parser,c.name,parent,childBuilder,cb,cbks:_*)
  }
}
