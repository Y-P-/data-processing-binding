package loader.core

import scala.reflect.ClassTag
import exceptions._
import reflect.runtime.universe.TypeTag
import loader.core.events.Event

object definition {
  import scala.language.implicitConversions
  
  //the compiler seems to require a little help, so this class seems necessary
  //we also need it to build fake Tops for includes
  protected abstract class Top[-Kind,+D<:Def](val processor:D, kindClass:Class[_<:Kind]) {
    def init:processor.Parser=>processor.Element
    def mapper:(processor.Element,Kind)=>processor.Kind  //cannot be argument because of processor dependance
    def map(elt:processor.Element,s:Kind):processor.Kind = if (mapper==null) s.asInstanceOf[processor.Kind] else mapper(elt,s)
    def check(p:ParserBuilder) = if (kindClass!=null && p.kindClass!=null && !kindClass.isAssignableFrom(p.kindClass)) throw new IncludeException(3,p.kindClass,kindClass)
  }
  
  /** This serves as a base for Status.
   *  In turn, Status is a class that serves as intermediary to build a child item from a parent item.
   */
  trait Status {
    def name:String
  }

  /** Defines the pattern through which we turn the sequence of parsed items into a structure with depth.
   *  The process involves 'stacking' data structures (Element), each referring to its parent.
   *  This trait only establishes the broad lines ; further refinements will turn this into usable code.
   *  @see Core    for a very basic implementation where elements only have names
   *  @see ExtCore for a richer implementation where elements can contain additional data
   *  @see CtxCore for a very rich implementation based on prior knowledge of the expected structure (using context)
   */
  trait Def { selfDef=>
    type Kind>:Null
    type Ret
    type Element  >: Null <: Elt
    type Parser   <: ParserBuilder#Impl
    type Status   >: Null <:definition.Status
    type UserCtx = UserContext[Element]
    type Cbk     = callbacks.Callback[Element,Status,Ret,Kind]
    type Cbks    = callbacks.Callbacks[Element,Status,Ret,Kind]
    type Bld     = EltBuilder
    
    //these do not have to be filled up; they will allow a runtime check on include if they are.
    def procClass = getClass
    def parsClass:Class[_<:ParserBuilder#Impl]
    def kindClass:Class[Kind]
    
    /** The standard, elementary methods dealing with parser events ; a subclass will usually refine this
     *  The order in which these methods are executed is:
     *  - parent executes onName ; child doesn't yet exist
     *  - child is created by the appropriate motor : onBeg is invoked
     *  - child may either be a terminal ; onVal is invoked when the value is received
     *  - or it maybe a container, in which case it will receive parents events (onName,onChild),
     *    until it is finished and onEnd is invoked.
     *  - parent executes onChild.
     *  They are protected because users have little reason to invoke these directly! See below onName0/onEnd0.
     *  However, these methods are important because they provide the entries for callbacks and understanding
     *  them is necessary to coding complex callbacks.
     */
    trait BaseElt {
      type Def      = selfDef.type
      type Ret      = selfDef.Ret
      type Status   = selfDef.Status
      type Element  = selfDef.Element
      type Bld      = selfDef.Bld
      type Kind     = selfDef.Kind
      type Parser   = selfDef.Parser
      def parser : Parser
      def definition:Def.this.type = Def.this
      protected def onName(name: String): Status
      protected def onBeg(): Unit
      protected def onVal(v: Kind): Ret
      protected def isInclude(v: Kind): Boolean
      protected def onInclude(v: Kind): Ret
      protected def onEnd(): Ret
      protected def onChild(child: Element, r: Ret): Unit
    }
    
    /** Reason for which Impl exists.
     *  Defines how the parser events are processed.
     *  Holds the data necessary to process these events.
     *  You probably don't want to ever override any of these methods.
     */
    abstract class Elt extends Traversable[Element] with BaseElt { self:Element=>
      def myself:Element = this
      import scala.reflect._
      /** Context for use */
      def userCtx:UserCtx
      /** Fields */
      def parent : Element  //parent item
      def name   : String   //element name
      def parser : Parser   //parser creating that element
      protected def parser_=(parser:Parser):Unit //parser has write access for handling includes
      /** Builder for children elements. builder should stay a def and point to the companion object to prevent bloating. */
      def childBuilder:Bld
      /** building a child spawning children of a possibly different nature */
      def build(p:Parser, s:Status, b:Bld):Element = childBuilder(p, self, s, b)
      /** building a child spawning children of the same nature */
      def build(p:Parser, s:Status):Element = build(p, s, childBuilder)
      
      /** builds the qualified name for the output */
      def qName = userCtx.qName(this)
      /** builds the local name for the output */
      def localName = userCtx.localName(this)
      //handle an event:
      // - ignore if no handler defined
      // - do nothing if handler not configured for that event
      def apply(evt:Event) = userCtx.eventHandler match {
        case null =>
        case h    => h.applyOrElse((this,evt),(x:(Element,Event))=>())
      }
      
      /** The push/pull interface on the processor side
       */
      def pull()         = parent.onChild(this, onEnd())
      def push(n:String) = { val c=build(parser,onName(n),childBuilder); c.onBeg(); c }
      def pull(v:Kind)   = { parent.onChild(this, if (isInclude(v)) onInclude(v) else onVal(v)) }
      
      /** standard invoker, used on the top level element */
      def invoke(f: =>Unit): Ret = {
        if (!isInclude) onBeg()  //onBeg method as already been called on included elements
        f
        onEnd() //note that for included elements, the onBeg was called using the top parser when the onEnd is called using the bottom one.
      }
      
      /** Used to change the parser during an include. */
      //creates a Top from this element for an include
      //callbacks are automatically carried over
      //!! Mutation: we change the current parser for this element
      //the parser will do some runtime checks to ensure that the processor (Element) and parser (from P) can work together.
      //the casts that follow will then work.
      //We must check:
      // - we have a transformer function from the the new parser kind to the current kind
      // - we use a processor supported by the new parser and that the new parser is accepted by this processor.
      protected def asTop(x:ParserBuilder#Executor)(mapper:(Element,x.builder.Kind)=>Kind) = new Top[x.builder.Kind](mapper,x.builder.kindClass,childBuilder,null,null) {
        override def init:Parser=>processor.Element = (p) => { self.parser = p; self }
      }.asInstanceOf[x.builder.BaseProcessor#Top[x.builder.Kind]]
      
      /** Handles an include ; this should be called in onVal(String) whenever necessary. */
      protected def onInclude(s:Kind): Ret = {
        val old = parser
        try {
          //We are going to spawn a parser executor out of nowhere.
          //There is no reason that it can accept the current Processor and we have to check this and then cast as appropriate (see asInclude).
          val i = userCtx.solveInclude(this)(s)                      //builds the relevant executor
          i(asTop(i)(userCtx.getMapper(this)(i))).asInstanceOf[Ret]  //run it with this element as top element; The return cast is always true because the element we are working on is still in the same class as this
        } finally {
          parser = old
        }
      }
      /** Checks when a given value received on this element is to be solved as an include. */
      protected def isInclude(s:Kind):Boolean = userCtx.isInclude(this)(s)
      
      //XXX heck! what was this ????
      //abstract override protected def onVal(s:String): Ret = if (isInclude(s:String)) doInclude(s) else super.onVal(s)
      
      /** Some convenient methods.
       *  Methods prefixed by g are general and use up the full parent chain.
       *  By contrast, the non preficed method only use the chain with items of the same
       *  kind ; this is the most common occurence.
       */
      def isRoot: Boolean = parent==null        //head of stack
      def isInclude: Boolean = parent match {   //head of sub-stack (i.e. include)
        case null    => false
        case p       => p.parser != parser
      }
      //iterator on the elements forming the full chain from this element to the top
      def toHead:Iterator[Element] = new Iterator[Element] {
        private var cur = self
        def hasNext: Boolean = cur!=null
        def next: Element = { val c=cur; cur=parent; c }
      }
      //iteration on the elements forming the full chain to this element starting from the top
      def foreach[U](f:Element=>U):Unit = { if (parent!=null) parent.foreach(f); f(self) }
      def iter[U](f:Element=>U):Traversable[Element] = new Traversable[Element] {
        def foreach[U](f:(Element)=>U) = self.foreach(f)
      }
      /** Prints the stack */
      def print(out:java.io.Writer):Unit = foreach(e=>out.write(s".${e.name}"))
      override def toString = { val s=new java.io.StringWriter; print(s); s.toString }      
    }
    
    class Top[-K](val mapper:(Element,K)=>Kind,kindClass:Class[_<:K],builder:EltBuilder,s:Status,cbks: Cbks*) extends definition.Top[K,Def.this.type](Def.this,kindClass) {
      def init:Parser=>processor.Element = if (cbks.isEmpty) builder(_,s) else builder(_,s,cbks:_*)
    }
    
    /** Defines how Element are built.
     *  - apply(Element,Status,Bld)           is the nominal builder
     *  - apply(Element,Status,Bld,Cbks*)     is the nominal builder in the presence of callbacks trees
     *  - apply(Element,Status,Bld,Cbk,Cbks*) should likely only be used internally
     *  Other methods are:
     *  - apply(Status,Bld)                   a builder for a root element (no parent)
     *  - apply(Status)                       a builder for a root element (no parent) which uses the current builder for its children
     */
    abstract class EltBuilder {
      def apply(s: Status):Top[Kind]                                                               = new Top(null.asInstanceOf[(Element,Kind)=>Kind],kindClass,this,s)
      def apply(s: Status, cbks: Cbks*):Top[Kind]                                                  = new Top(null.asInstanceOf[(Element,Kind)=>Kind],kindClass,this,s,cbks:_*)
      def apply[K](mapper:(Element,K)=>Kind, kindClass:Class[_<:K], s: Status):Top[K]              = new Top(mapper,kindClass,this,s)
      def apply[K](mapper:(Element,K)=>Kind, kindClass:Class[_<:K], s: Status, cbks: Cbks*):Top[K] = new Top(mapper,kindClass,this,s,cbks:_*)
      def apply(p: Parser, s: Status): Element                                                     = apply(p,null,s,this)
      def apply(p: Parser, s: Status, childBuilder: Bld): Element                                  = apply(p,null,s,childBuilder)
      def apply(p: Parser, s: Status, cbks: Cbks*): Element                                        = apply(p,s,this,cbks:_*)
      def apply(p: Parser, s: Status, childBuilder: Bld, cbks: Cbks*): Element                     = WithCallbacks(p,null,s,cbks,childBuilder,childBuilder)
      def apply(p: Parser, parent: Element, s: Status, childBuilder: Bld): Element
      def apply(p: Parser, parent: Element, s: Status, childBuilder: Bld, cbks: Cbks*): Element with WithCallbacks
      def apply(p: Parser, parent: Element, s: Status, childBuilder: Bld, cb: Cbk, cbks: Cbks*): Element with WithCallback
    }

    /** Modifies the current element behavior by using a callback
     */
    trait WithCallback extends WithCallbacks { this: Element =>
      /** When handling Callbacks, we will want to reach the parent beahviour. */
      val cb: callbacks.Callback[Element,Status,Ret,Kind] //the callback for the current element
      val cbx = cb(this)
      abstract override protected def onName(name: String): Status          = if (cbx==null) super.onName(name) else cbx.onName(name, super.onName)
      abstract override protected def onBeg(): Unit                         = if (cbx==null) super.onBeg() else cbx.onBeg(super.onBeg)
      abstract override protected def onVal(v: Kind): Ret                   = if (cbx==null) super.onVal(v) else cbx.onVal(v,super.onVal)
      abstract override protected def isInclude(v: Kind): Boolean           = if (cbx==null) super.isInclude(v) else cbx.isInclude(v,super.isInclude)
      abstract override protected def onInclude(v: Kind): Ret               = if (cbx==null) super.onInclude(v) else cbx.onInclude(v,super.onInclude)
      abstract override protected def onEnd(): Ret                          = if (cbx==null) super.onEnd() else cbx.onEnd(super.onEnd)
      abstract override protected def onChild(child: Element, r: Ret): Unit = if (cbx==null) super.onChild(child,r) else cbx.onChild(child, r, super.onChild)
    }
    /** Modifies the current element to manage a callback tree
     *  Children are built according to the following rules:
     *  - if no callback subtree applies for the child, the child returns to the base implementation, removing any overhead for callback
     *  - if a callback tree is present for the child, but no callback applies, the child uses the WithCallbacks trait
     *    this causes additional data to be carried, and some overhead when building children
     *  - if a callback tree is present for the child, and a callback applies, the child uses the WithCallback trait
     *    this causes the same overhead as the previous case; in addition, the callback is carried and base methods pass through it (onBeg etc...)
     */
    trait WithCallbacks extends Elt { this: Element =>
      protected[this] def cbks: Seq[Cbks] //current callbacks trees (for children)
      override def build(p: Parser, s: Status, b:Bld): Element = WithCallbacks(p,this,s,cbks,childBuilder,b)
    }
    object WithCallbacks {
      /** Analyzes a callbacks sequence to know:
       *  1) whether it applies to the current item
       *  2) what sub sequence may apply to children
       */
      def apply(p: Parser, parent:Element, s:Status, cbks:Seq[Cbks], builder:EltBuilder, childBuilder:Bld): Element = {
        if (cbks.length == 1) {
          //first, the case where the sequence is one element only.
          //it's very common, and should be optimized! it's furthermore much easier to read!
          cbks.head.get(s.name) match {
            case None => builder(p,parent,s,childBuilder) //no subtree ? get rid of the extra callback data and associated code
            case Some(c) => c.cur match {
              case None => builder(p,parent,s,childBuilder,cbks:_*)
              case Some(cb) => builder(p,parent,s,childBuilder,cb,cbks:_*)
            }
          }
        } else {
          //that one is a little tricky; first build the next sequence of callback trees, extracted from cbks
          val r = for (x <- cbks; y <- x.get(s.name)) yield y
          //if empty, return to the no callback version
          if (r.isEmpty) builder(p,parent,s,childBuilder)
          else {
            //otherwise, create the sequence of actual callbacks for this element
            val c = (for (x <- r; y <- x.cur) yield y)
            //if empty, proceed with the builder with non local callback
            if (c.isEmpty) builder(p,parent,s,childBuilder,r:_*)
            //otherwise, proceed by combining the local callbacks together to compute the final callback to apply
            else builder(p,parent,s,childBuilder,c.reduce(_(_)),r:_*)
          }
        }
      }
    }
    /** Conversion to traversable. */
    implicit def toTraversable[U](e:Element):Traversable[Element] = new Traversable[Element] {
      def foreach[U](f:(Element)=>U) = e.foreach(f)
    }
  }
  
  trait Impl extends Def { self=>
    /** An actual implementation class should extend this trait.
     *  All implementations based on the same core (i.e. extending this trait from the same instance)
     *  are interoperable.
     *  See examples for how to use this.
     */
    trait Impl {
      def builder:Bld       //an associated builder
    }
    
    //a factory for reading textual parameters
    def apply(pr: utils.ParamReader, userCtx:UserCtx):Impl
    
    /** Forwards the base methods to the upper layer.
     *  This causes a redirection to apply them, but usually has the immense advantage of fully defining the element by
     *  defining all behaviours. Using Motor makes it easier to define processors, all using a common element base.
     */
    trait Motor extends Impl { motor=>
      type Result
      type ElementBase<:Element
      // context fields for a motor
      def userCtx:UserCtx
      // Forwarded methods
      protected def onInit():Unit
      protected def onExit():Result
      protected def onName(self: Element, name: String): Status
      protected def onBeg(self: Element): Unit
      protected def onVal(self: Element, v: Kind): Ret
      protected def onEnd(self: Element): Ret
      protected def onChild(self: Element, child: Element, r: Ret): Unit
      // Element implementation : redirect calls
      trait Processor extends Elt { self:ElementBase=>
        def userCtx = motor.userCtx
        protected def onName(name: String): Status          = motor.onName(this,name)
        protected def onBeg(): Unit                         = motor.onBeg(this)
        protected def onVal(v: Kind): Ret                   = motor.onVal(this,v)
        protected def onEnd(): Ret                          = motor.onEnd(this)
        protected def onChild(child: Element, r: Ret): Unit = motor.onChild(this,child,r)
        /*
         * We have to manage the copy required when we have an include: the current element must be attached to
         * the upper parser, but also to the lower one! Thus, we must copy the current element be change the parser.
         * Several methods are possible to do this.
         * - creating the appropriate method for each implementing class; extremely tedious and not scalable
         * - clone + java reflexion : fails (these objects are too complex to locate the parser field!)
         * - clone + scala reflexion : works (snipet below), but awful perfs at 'cm.reflect(r)'
         * - clone + inner mutable (parser0) that shadows parser
         * However problems arise with other fields that depend on the current element but still refer to the old
         * one, for example cb (callback.) ; the proper solution for this would be to rebuild a new object from
         * scratch with the appropriate parser ; but this has its own drawbacks in case some of the methods invoked
         * have side effects (such as getData.)
         * Finally, is seems safer to make Elt mutable on parser as this is the solution that seems to be the less
         * restrictive and the most efficient. One must not forget to restore the old parser!
         * 
         * Snipet for scala reflexion:
         *   import scala.reflect.runtime.{ currentMirror => cm }
         *   import scala.reflect.runtime.universe._
         *   val im = cm.reflect(r)
         *   val termSymb = typeOf[ElementBase].declaration(newTermName("parser")).asTerm
         *   val fm = im.reflectField(termSymb)
         *   fm.set(parser)
         */
        protected var parser0:Parser
        def parser = parser0
        protected def parser_=(parser:Parser):Unit = parser0=parser
      }  
    }
    
    implicit def toBuilder(impl:Impl):Bld = impl.builder
  }

}