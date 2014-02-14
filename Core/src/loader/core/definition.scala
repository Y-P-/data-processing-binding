package loader.core

import scala.reflect.ClassTag
import exceptions._
import reflect.runtime.universe.TypeTag
import loader.core.events.Event

object definition {
  import scala.language.implicitConversions
  
  //val isIncludeException = new Exception { override def fillInStackTrace = this }
  
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
    type PB[+k] <: ParserBuilder.This[this.type,k]
    type Parser = PB[Any]#Parser //most of the time, we don't care about the kind produced by the parser
    type Kind>:Null
    type Ret
    type Element  >: Null <: Elt
    type Status   >: Null <:definition.Status
    type UserCtx = UserContext[Element]
    type Cbk     = callbacks.Callback[Element,Status,Ret,Kind]
    type Cbks    = callbacks.Callbacks[Element,Status,Ret,Kind]
    type Bld     = EltBuilder
    
    def parsClass:Class[_<:ParserBuilder#Parser]
    def kindClass:Class[_<:Kind]
    
    implicit val kindTag:ClassTag[Kind] = ClassTag(kindClass)
    
    /** Binds together a processor (type Def), a parser (through the init method which creates that processor initial element),
     *  and a mapper method (which bridges the gap between the parser produced type and the processor expected type.)
     *  @param K, the kind of data produced by the parser
     *  @param P, the parser class used, that produces tokens of class K
     *  @param mapper, a method that converts K to Kind, the data type expected by the processor ; if null, we assume K=Kind
     *  @param init, the method that spawns the top element ; this is usually produced through the appropriate call to an EltBuilder
     */
    final class Launcher[-K:ClassTag] (mapper:(Element,K)=>Kind, init: Parser=>Element) {
      final type Element = Def.this.Element
      val kindClass = implicitly[ClassTag[K]].runtimeClass
      def map(elt:Element,s:K):Kind = if (mapper==null) s.asInstanceOf[Kind] else mapper(elt,s)
      /** Creates the top Element for the given parser. This indeed does the binding between parser and processor. */
      def apply(p:PB[K]#Parser):Element = {
        //XXX change the exception (not include!), see the associated messages
        //XXX check K vs Kind ?
        //dynamic check to ensure that the parser can run with the processor.
        //usually, the compile time check is sufficient, but not in case of includes.
        // 1) Check parser is acceptable for processor
        //if (p.parsClass!=null && parsClass!=null && !(parsClass.isAssignableFrom(p.parsClass)))
        //  throw new IncludeException(1,p.parsClass,procClass)
        // 2) Check processor is acceptable for parser
        //if (p.procClass!=null && procClass!=null && !(p.procClass.isAssignableFrom(procClass)))
        //  throw new IncludeException(2,procClass,p.procClass)
        // 3) Check parser kind is compatible with Top kind
        //if (kindClass!=null && p.kindClass!=null && !kindClass.isAssignableFrom(p.kindClass))
        //  throw new IncludeException(3,p.kindClass,kindClass)
        init(p)
      }
      def apply(p:PB[K])(f:p.Parser=>Unit):Ret = {
        val r = p(this)
        r.invoke(f(r))
      }
      /** Loading from a Reader. */
      def run(p:PB[K], in: java.io.Reader) = this(p)(_.read(in))
      /** Loading from an uri with encoding possibly specified. */
      def run(p:PB[K], uri: java.net.URI, encoding: String) = this(p)(_.read(uri, encoding))
    }
    
    /** Creates the association that binds this processor with a Parser implementation.
     */
    def apply[K:ClassTag](mapper:(Element,K)=>Kind,builder:EltBuilder,s:Status,cbks:Cbks*):Launcher[K] =
      new Launcher[K](mapper,(p:Parser)=>if (cbks.isEmpty) builder(p,s) else builder(p,s,cbks:_*))
    def apply[K:ClassTag](mapper:(Element,K)=>Kind,init:Parser=>Element):Launcher[K] =
      new Launcher[K](mapper,init)

    
    
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
      protected def onInit(): Unit                             //called on all elements on creation
      protected def onBeg(): Unit                              //called on beginning of a struct
      protected def onVal(v: Kind): Ret                        //called when receiving a value
      protected def onInclude(v: Kind, e: ()=>Ret): Ret        //called when receiving an include (as defined by the EltCtx solveInclude)
      protected def onEnd(): Ret                               //called at the end of a struct
      protected def onChild(child: Element, r: Ret): Unit      //called when a struct is receiving a child value
      def incl[K:ClassTag,R>:Ret] (
              mapper:(Element,K)=>Kind,
              retMapper:R=>Ret
            ):()=>Ret
    }
    
    /** Reason for which Impl exists.
     *  Defines how the parser events are processed.
     *  Holds the data necessary to process these events.
     *  You probably don't want to ever override any of these methods.
     */
    trait Elt extends Traversable[Element] with BaseElt { self:Element=>
      implicit val eltCtx = userCtx(this)
      def myself:Element = this
      /** Context for use */
      def userCtx:UserCtx
      /** Fields */
      def parent : Element  //parent item
      def name   : String   //element name
      def parser : Parser   //parser creating that element: Beware => this can change in case of includes
      protected[Def] def parser_=(parser:Parser):Unit //parser has write access for handling includes
      /** Builder for children elements. builder should stay a def and point to the companion object to prevent bloating. */
      def childBuilder:Bld
      /** building a child spawning children of a possibly different nature */
      def build(p:Parser, s:Status, b:Bld):Element = childBuilder(p, self, s, b)
      /** building a child spawning children of the same nature */
      def build(p:Parser, s:Status):Element = build(p, s, childBuilder)
      
      /** builds the qualified name for the output */
      def qName = eltCtx.qName
      /** builds the local name for the output */
      def localName = qName.local
      /** current depth in the hierarchy; 0 is top */
      def depth:Int = if (parent==null) 0 else parent.depth+1
      //handle an event:
      // - ignore if no handler defined
      // - do nothing if handler not configured for that event
      def apply(evt:Event) = userCtx.eventHandler match {
        case null =>
        case h    => h.applyOrElse((this,evt),(x:(Element,Event))=>())
      }
      
     /** Builds an include on the current element, i.e. a sub-engine that works from the current processor state.
      *  When the sub-engine exits, the processor is left in the same state as it entered (i.e. same current element.)
      *  @param K, the kind of data produced by the sub-parser
      *  @param R, the kind of result produced by the sub-engine
      *  @param mapper, a method to coerce K to Kind (expected by the current engine)
      *  @param exc, an executor that contains the data to include
      *  @param retMapper, a method to coerce return data
      */
      def incl[K:ClassTag,R>:Ret] (
              mapper:(Element,K)=>Kind,
              retMapper:R=>Ret
            ):()=>Ret = ()=>{
        val old = parser
        try {
          //create a Launcher using this processor and the current element
          //replace the current element parser with the new one
          //then start the executor and coerce the returned value to the appropriate Ret value
          val r:R = null.asInstanceOf[R]//exc(selfDef(mapper,(p:Parser)=>{parser=p; self}))
          if (retMapper==null) r.asInstanceOf[Ret] else retMapper(r)
        } finally {
          parser=old
        }        
      }
      
      /** Ensure the call to doBeg is done as late as possible, but in time. begDone ~ lazy val */
      private var begDone=false
      private def doBeg():Unit = if (!begDone) { begDone=true; if (parent!=null) parent.doBeg; onBeg; }
      
      /** The push/pull interface on the processor side
       */
      def push(n:String) = { val c=build(parser,onName(n),childBuilder); c.onInit(); c }
      def pull()         = { doBeg(); parent.onChild(this, onEnd()) }
      def pull(v:Kind)   = {
        parent.doBeg
        parent.onChild(this,
          eltCtx.solver(v) match {
            case null => onVal(v)
            case i    => onInclude(v,i)
          })
      }
      
      /** standard invoker, used on the top level elements */
      def invoke(f: =>Unit): Ret = {
        if (!isInclude) onBeg()  //onBeg method as already been called on included elements
        f
        onEnd() //note that for included elements, the onBeg was called using the top parser when the onEnd is called using the bottom one.
      }
      
      /** the standard processing for includes. This likely should not be altered. */
      protected def onInclude(v: Kind, e: ()=>Ret): Ret = e()
            
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
      
    /** Defines how Element are built.
     *  - apply(Element,Status,Bld)           is the nominal builder
     *  - apply(Element,Status,Bld,Cbks*)     is the nominal builder in the presence of callbacks trees
     *  - apply(Element,Status,Bld,Cbk,Cbks*) should likely only be used internally
     *  Other methods are:
     *  - apply(Status,Bld)                   a builder for a root element (no parent)
     *  - apply(Status)                       a builder for a root element (no parent) which uses the current builder for its children
     */
    abstract class EltBuilder {
      def apply(s: Status)                                                          = Def.this.apply(null.asInstanceOf[(Element,Kind)=>Kind],this,s)
      def apply(s: Status, cbks: Cbks*)                                             = Def.this.apply(null.asInstanceOf[(Element,Kind)=>Kind],this,s,cbks:_*)
      def apply[K:ClassTag](mapper:(Element,K)=>Kind, s: Status)                    = Def.this.apply(mapper,this,s)
      def apply[K:ClassTag](mapper:(Element,K)=>Kind, s: Status, cbks: Cbks*)       = Def.this.apply(mapper,this,s,cbks:_*)
      def apply(p: Parser, s: Status): Element                                      = apply(p,null,s,this)
      def apply(p: Parser, s: Status, childBuilder: Bld): Element                   = apply(p,null,s,childBuilder)
      def apply(p: Parser, s: Status, cbks: Cbks*): Element                         = apply(p,s,this,cbks:_*)
      def apply(p: Parser, s: Status, childBuilder: Bld, cbks: Cbks*): Element      = WithCallbacks(p,null,s,cbks,childBuilder,childBuilder)
      def apply(p: Parser, parent: Element, s: Status, childBuilder: Bld): Element
      def apply(p: Parser, parent: Element, s: Status, childBuilder: Bld, cbks: Cbks*): Element with WithCallbacks
      def apply(p: Parser, parent: Element, s: Status, childBuilder: Bld, cb: Cbk, cbks: Cbks*): Element with WithCallback
    }

    /** Modifies the current element behavior by using a callback
     */
    trait WithCallback extends WithCallbacks { this: Element =>
      /** When handling Callbacks, we will want to reach the parent behaviour. */
      val cb: callbacks.Callback[Element,Status,Ret,Kind] //the callback for the current element
      val cbx = cb(this)
      abstract override protected def onName(name: String): Status               = if (cbx==null) super.onName(name) else cbx.onName(name, super.onName)
      abstract override protected def onBeg(): Unit                              = if (cbx==null) super.onBeg() else cbx.onBeg(super.onBeg)
      abstract override protected def onVal(v: Kind): Ret                        = if (cbx==null) super.onVal(v) else cbx.onVal(v,super.onVal)
      abstract override protected def onInclude(v: Kind, x: ()=>Ret): Ret        = if (cbx==null) super.onInclude(v,x) else cbx.onInclude(v, x, super.onInclude)
      abstract override protected def onEnd(): Ret                               = if (cbx==null) super.onEnd() else cbx.onEnd(super.onEnd)
      abstract override protected def onChild(child: Element, r: Ret): Unit      = if (cbx==null) super.onChild(child,r) else cbx.onChild(child, r, super.onChild)
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
      
      protected def onInit():Unit
      protected def onExit():Result
      // Forwarded methods
      protected def onName(self: Element, name: String): Status
      protected def onInit(self: Element):Unit
      protected def onBeg(self: Element): Unit
      protected def onVal(self: Element, v: Kind): Ret
      protected def onEnd(self: Element): Ret
      protected def onChild(self: Element, child: Element, r: Ret): Unit
      // Element implementation : redirect calls
      trait Processor extends Elt { self:ElementBase=>
        def userCtx = motor.userCtx
        protected def onName(name: String): Status          = motor.onName(this,name)
        protected def onInit(): Unit                        = motor.onInit(this)
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
        protected[this] var parser0:Parser
        def parser = parser0
        protected[definition] def parser_=(parser:Parser):Unit = parser0=parser
      }  
    }
    
    implicit def toBuilder(impl:Impl):Bld = impl.builder
  }

  type D0[-i,+r] = Def { type Parser>:i; type Ret<:r }
}