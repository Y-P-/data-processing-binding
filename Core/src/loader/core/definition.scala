package loader.core

import scala.reflect.ClassTag
import exceptions._
import reflect.runtime.universe.TypeTag
import loader.core.events.Event

object definition {
  import scala.language.implicitConversions
  
  /** This serves as a base for Status.
   *  In turn, Status is a class that serves as intermediary to build a child item from a parent item.
   */
  trait Status[+K>:Null] {
    def key:K
  }

  /** Defines the pattern through which we turn the sequence of parsed items into a structure with depth.
   *  The process involves 'stacking' data structures (Element), each referring to its parent.
   *  This trait only establishes the broad lines ; further refinements will turn this into usable code.
   *  @see Core    for a very basic implementation where elements only have names
   *  @see ExtCore for a richer implementation where elements can contain additional data
   *  @see CtxCore for a very rich implementation based on prior knowledge of the expected structure (using context)
   */
  trait Processor { selfDef=>
    type Kind>:Null    //the kind of data processed by this processor
    type Key>:Null     //the keys recognized by this processor ; note that key.toString should be cached inside because heavy use of it is done
    type Ret
    type Element >: Null <: Elt
    type BaseParser <: ParserBuilder
    type Status >: Null <:definition.Status[Key]
    
    type UserCtx = UserContext[selfDef.type]    
    type Parser  = BaseParser#Parser[_,_,Ret]
    type Cbk     = callbacks.Callback[Element,Status,Ret,Key,Kind]
    type Cbks    = callbacks.Callbacks[Element,Status,Ret,Key,Kind]
    type Bld     = EltBuilder
    
    val noKey:Key
    
    /** This is used to define the builder for an implementation.
     *  It is used to start a processor. 
     */
    trait Launcher {
      val proc:Processor.this.type = Processor.this
      def myself:proc.Launcher = this  //prevents some non necessary casts
      def builder:Bld
              
      /** Specific factories.
       *  A Parser=>Element function is expected: it spawns the Top element for a given Parser.
       */
      def apply(s:Status,cbks:Cbks*):Parser=>Element = p=>if (cbks.isEmpty) builder(p,s) else builder(p,s,cbks:_*)
      def apply(s:Status):Parser=>Element            = builder(_,s)
    }
        
    /** An object that is attached to a parser object (that has been pushed, to the contrary of
     *  values that are pulled.) One such object is spawned for each parser object, to process
     *  it accordingly to the processor's requirements.
     */
    trait Elt extends Launcher with Traversable[Element] { self:Element=>
      override def myself:proc.Element = this  //prevents some non necessary casts
      implicit val eltCtx = userCtx(this)
      /** Context for use */
      def userCtx:UserCtx
      /** Fields */
      def parent : Element  //parent item
      def key    : Key      //element key
      def name   : String  = key.toString  //element name; provided for simplicity
      def parser : Parser   //parser creating that element: Beware => this can change in case of includes
      protected[core] def parser_=(parser:Parser):Unit //parser has write access for handling includes
      /** Builder for children elements. builder should stay a def and point to the companion object to prevent bloating. */
      def builder:Bld
      /** building a child spawning children of the same nature; you must call this method because it can be overriden (callbacks) */
      def build(p:Parser, s:Status):Element = builder(p, this, s)
      
      /** The standard, elementary methods dealing with parser events.
       *  The order in which these methods are executed is:
       *  - parent executes onName ; child doesn't yet exist
       *  - child is created by the appropriate builder : onBeg is invoked
       *  - child may either be a terminal ; onSolver is checked. If not null, it is invoked, otherwise onVal is invoked.
       *  - or it maybe a container, in which case it will receive parents events (onName,onChild),
       *    until it is finished and onEnd is invoked.
       *  - parent executes onChild.
       *  They are protected because users have little reason to invoke these directly! See below onName0/onEnd0.
       *  However, these methods are important because they provide the entries for callbacks and understanding
       *  them is necessary to coding complex callbacks.
       */
      protected def onName(key: Key): Status
      protected def onInit(): Unit                           //called on all elements on creation
      protected def onBeg(): Unit                            //called once only, before any other call within struct, as late as possible (i.e. only when receiving the first child, or when closing the struct)
      protected def onVal(v: Kind): Ret                      //called when receiving a value
      protected def onEnd(): Ret                             //called at the end of a struct
      protected def onChild(child: Element, r: Ret): Unit    //called when a struct is receiving a child value
      protected def onSolver(v: Kind, e: ()=>Ret): Ret = e() //called when resolving a value (as defined by the EltCtx solver) ; you had better know what you do if you override this
      
      /** current depth in the hierarchy; 0 is top */
      def depth:Int = if (parent==null) 0 else parent.depth+1
      //handle an event:
      // - ignore if no handler defined
      // - do nothing if handler not configured for that event
      def apply(evt:Event) = userCtx.eventHandler match {
        case null =>
        case h    => h.applyOrElse((this,evt),(x:(Element,Event))=>())
      }
      
      /** Ensure the call to doBeg is done as late as possible, but in time. */
      private[this] var begDone=false
      private def doBeg():Unit = if (!begDone) { begDone=true; if (parent!=null) parent.doBeg; onBeg; }
      
      /** The push/pull interface on the processor side
       */
      def push(n:Key):Processor.this.Element = { val c=build(parser,onName(n)); c.onInit(); c }
      def pull()         = { doBeg(); parent.onChild(this, onEnd()) }
      def pull(v:Kind)   = {
        parent.doBeg
        parent.onChild(this,
          eltCtx.solver(v) match {
            case null => onVal(v)
            case i    => onSolver(v,i)
          })
      }
      
      /** standard invoker, used on the top level elements */
      def invoke(f: =>Unit): Ret = {
        if (!isInclude) onBeg()  //onBeg method as already been called on included elements
        f
        onEnd() //note that for included elements, the onBeg was called using the top parser when the onEnd is called using the bottom one.
      }
            
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
          
    /** Defines how Element are built in various contexts.
     *  - the first four methods define Top builders
     *  - the other define child builders within an already existing processor
     */
    abstract class EltBuilder {
      def apply(p: Parser, s: Status): Element                = apply(p,null,s)
      def apply(p: Parser, s: Status, cbks: Cbks*): Element   = apply(p,null,s,cbks:_*)
      def apply(p: Parser, parent: Element, s: Status): Element
      def apply(p: Parser, parent: Element, s: Status, cbks: Cbks*): Element with WithCallbacks
      def apply(p: Parser, parent: Element, s: Status, cb: Cbk, cbks: Cbks*): Element with WithCallback
    }

    /** Modifies the current element behavior by using a callback
     */
    trait WithCallback extends WithCallbacks { this: Element =>
      /** When handling Callbacks, we will want to reach the parent behaviour. */
      val cb: callbacks.Callback[Element,Status,Ret,Key,Kind] //the callback for the current element
      val cbx = cb(this)
      abstract override protected def onName(key: Key): Status              = if (cbx==null) super.onName(key) else cbx.onName(key, super.onName)
      abstract override protected def onBeg(): Unit                         = if (cbx==null) super.onBeg() else cbx.onBeg(super.onBeg)
      abstract override protected def onVal(v: Kind): Ret                   = if (cbx==null) super.onVal(v) else cbx.onVal(v,super.onVal)
      abstract override protected def onSolver(v: Kind, x: ()=>Ret): Ret    = if (cbx==null) super.onSolver(v,x) else cbx.onSolver(v, x, super.onSolver)
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
      override def build(p: Parser, s: Status): Element = WithCallbacks(p,this,s,cbks,builder)
    }
    object WithCallbacks {
      /** Analyzes a callbacks sequence to know:
       *  1) whether it applies to the current item
       *  2) what sub sequence may apply to children
       */
      def apply(p: Parser, parent:Element, s:Status, cbks:Seq[Cbks], builder:EltBuilder): Element = {
        if (cbks.length == 1) {
          //first, the case where the sequence is one element only.
          //it's very common, and should be optimized! it's furthermore much easier to read!
          cbks.head.get(s.key.toString) match {
            case None => builder(p,parent,s) //no subtree ? get rid of the extra callback data and associated code
            case Some(c) => c.cur match {
              case None => builder(p,parent,s,cbks:_*)
              case Some(cb) => builder(p,parent,s,cb,cbks:_*)
            }
          }
        } else {
          //that one is a little tricky; first build the next sequence of callback trees, extracted from cbks
          val r = for (x <- cbks; y <- x.get(s.key.toString)) yield y
          //if empty, return to the no callback version
          if (r.isEmpty) builder(p,parent,s)
          else {
            //otherwise, create the sequence of actual callbacks for this element
            val c = (for (x <- r; y <- x.cur) yield y)
            //if empty, proceed with the builder with non local callback
            if (c.isEmpty) builder(p,parent,s,r:_*)
            //otherwise, proceed by combining the local callbacks together to compute the final callback to apply
            else builder(p,parent,s,c.reduce(_(_)),r:_*)
          }
        }
      }
    }
    /** Conversion to traversable. */
    implicit def toTraversable[U](e:Element):Traversable[Element] = new Traversable[Element] {
      def foreach[U](f:(Element)=>U) = e.foreach(f)
    }
  }
  
  /** A possible implementation for the framework.
   *  It redirects the element calls to the launcher class, which contains the processor logic.
   *  This lets define standard Element classes.
   */
  trait Impl extends Processor { self=>
    //a factory for reading textual parameters
    //there will be other, specific factories
    def apply(pr: utils.ParamReader, userCtx:UserCtx):Motor
    
    /** Forwards the base methods to the upper layer.
     *  This causes a redirection to apply them, but usually has the immense advantage of fully defining the element by
     *  defining all behaviours. Using Motor makes it easier to define processors, all using a common element base.
     *  It also makes it possible to easily subclass an implementation.
     */
    abstract class Motor extends super.Launcher { motor=>
      type Result
      def userCtx:UserCtx
      
      def onInit():Unit
      def onExit():Result
      // Forwarded methods
      def onName(self: Element, key: Key): Status
      def onInit(self: Element):Unit
      def onBeg(self: Element): Unit
      def onVal(self: Element, v: Kind): Ret
      def onEnd(self: Element): Ret
      def onChild(self: Element, child: Element, r: Ret): Unit
    }
    
    // Element implementation : redirect calls
    trait Elt extends super.Elt { this:Element=>
      val motor:Motor
      protected[this] var parser0:Parser
      def parser = parser0  //we would rather not have this var, but the alternative is not good either.
      protected[core] def parser_=(parser:Parser):Unit = parser0=parser      
      def userCtx = motor.userCtx
      def builder = motor.builder
      protected def onName(key: Key): Status              = motor.onName(this,key)
      protected def onInit(): Unit                        = motor.onInit(this)
      protected def onBeg(): Unit                         = motor.onBeg(this)
      protected def onVal(v: Kind): Ret                   = motor.onVal(this,v)
      protected def onEnd(): Ret                          = motor.onEnd(this)
      protected def onChild(child: Element, r: Ret): Unit = motor.onChild(this,child,r)
    }      
  }
}