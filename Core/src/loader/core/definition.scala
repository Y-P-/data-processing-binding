package loader.core

import exceptions._
import loader.core.events.Event
import scala.annotation.tailrec

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
  trait Processor {proc=>
    type Value>:Null   //the Value type of data processed by this processor
    type Key>:Null     //the keys recognized by this processor ; note that key.toString should be cached inside because heavy use of it is done
    type Ret
    type Status>:Null<:definition.Status[Key]
    type BaseParser <: ParserBuilder
    type UCtx[-P<:BaseParser]>:Null<:UsrCtx[P,this.type]
    protected type Element[X<:BaseParser with Singleton] <: Elt { type Builder=X }
    type Elt>:Null<:EltBase
        
    //useful type shortcuts
    type Cbk            = callbacks.Callback[Elt,Status,Ret,Key,Value]
    type Cbks           = callbacks.Callbacks[Elt,Status,Ret,Key,Value]
    type CbksEltBuilder = callbacks.CallbacksBuilder[Elt,Status,Ret,Key,Value]
    
    val noKey:Key
    //used for dynamic checks
    def baseParserClass:Class[BaseParser]
    def baseUCtxClass:Class[UCtx[_]]  //XXX can be a list of interfaces
    
    
  /** This implementation does a cast.
   *  It is has the advantage of reducing the element memory footprint by reaching into the parser (which is
   *  already itself referenced) ; it also will slightly improve performances by having one less field to fill in.
   *  It ensures that one of the conditions for using Efficient in parsers is met: indeed: parser.userCtx == elt.userCtx
   *  But it cannot ensure type security unless this type respects: UCtx[Builder] with Builder#UCtx[Processor.this.type]
   *  In effect, this cannot be written here without falling into the 'volatile type' trap.
   *  It will have to be ensured at the calling level.
   *  IMPORTANT: object run ensures this.
   *  Note: unused at this stage. The benefits do not seem to outweight the drawback of writing a whole special
   *        brand of Element implementations. 
   */
    trait Efficient {this:EltBase=>
      def userCtx:UCtx[Builder] = parser.userCtx.asInstanceOf[UCtx[Builder]]
    }
    trait Sure {this:EltBase=>
      val userCtx:UCtx[Builder]
    }
    
    /** An object that is attached to a parser object (that has been pushed, to the contrary of
     *  values that are pulled.) One such object is spawned for each parser object, to process
     *  it accordingly to the processor's requirements.
     */
    trait EltBase extends Traversable[Elt] { self:Elt=>
      final def myself:Elt { type Builder=self.Builder } = self //self cast
      type Builder <: BaseParser with Singleton
      val parser:Builder#Parser
      /** Context for use */
      def userCtx:UCtx[Builder]
      val eltCtx = userCtx(this)
      /** Fields */
      def parent : Elt      //parent item
      def key    : Key      //element key
      def name   : String = key.toString  //element name; provided for simplicity
      /** Builder for children elements. builder should stay a def and point to the companion object to prevent bloating. */
      def builder:EltBuilder
      /** building a child spawning children of the same nature; you must call this method because it can be overridden (callbacks) */
      def build(p:Builder#Parser, u:UCtx[Builder], s:Status):Elt = builder(p, u, this, s)
      /** the status for this element */
      def status:Status
      /** copies the element and assigns another compatible parser ; important to manage includes, especially for handling split elements/multiple redirections. */
      def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P }
      
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
      protected def onInit(): Unit                            //called on all elements on creation
      protected def onBeg(): Unit                             //called once only, before any other call within struct, as late as possible (i.e. only when receiving the first child, or when closing the struct)
      protected def onVal(v: Value): Ret                      //called when receiving a value
      protected def onEnd(): Ret                              //called at the end of a struct
      protected def onChild(child: Elt, r: Ret): Unit         //called when a struct is receiving a child value
      protected def onSolver(v: Value, e: ()=>Ret): Ret = e() //called when resolving a value (as defined by the EltCtx solver) ; you had better know what you do if you override this
      
      /** current depth in the hierarchy; 0 is top */
      def depth:Int = if (parent==null) 0 else parent.depth+1
      //handle an event:
      // - ignore if no handler defined
      // - do nothing if handler not configured for that event
      def apply(evt:Event) = eltCtx.eventHandler match {
        case null =>
        case h    => h.applyOrElse((this,evt),(x:(Elt,Event))=>())
      }
      /** element of rank n in the hierarchy ; 0 is the current element */
      @tailrec final def apply(n:Int):Elt = if (n==0) this else if (parent!=null) parent(n-1) else throw new IndexOutOfBoundsException
      
      /** Ensure the call to doBeg is done as late as possible, but in time. */
      private[this] var begDone=false
      private def doBeg():Unit = if (!begDone) { begDone=true; if (parent!=null) parent.doBeg; onBeg; }
            
      /** The push/pull interface on the processor side
       */
      def push(n:Key):Elt = { val c=build(parser,userCtx,onName(n)); c.onInit(); c }
      def pull()          = { doBeg(); parent.onChild(this,onEnd()) }
      def pull(v:Value)   = {
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
      
      /** Returns a view to the name sequence from the current item.
       *  Especially useful for matching names in context.
       */
      def names() = toHead.view.map(_.name)
            
      /** Some convenient methods.
       *  Methods prefixed by g are general and use up the full parent chain.
       *  By contrast, the non preficed method only use the chain with items of the same
       *  Value ; this is the most common occurence.
       */
      def isRoot: Boolean = parent==null        //head of stack
      def isInclude: Boolean = parent!=null && !(parent.parser eq parser)
      //sequence on the elements forming the full chain from this element to the top
      def toHead:Seq[Elt] = new Seq[Elt] {
        lazy val length = depth+1
        def iterator = new Iterator[Elt] {
          private var cur = EltBase.this
          def hasNext: Boolean = cur!=null
          def next: Elt = { val c=cur; cur=cur.parent; c }
        }
        def apply(idx: Int): Elt = EltBase.this(idx)
      }
      //iteration on the elements forming the full chain to this element starting from the top
      def foreach[U](f:Elt=>U):Unit = { if (parent!=null) parent.foreach(f); f(this) }
      def iter:Traversable[Elt] = new Traversable[Elt] {
        def foreach[U](f:Elt=>U) = EltBase.this.foreach(f)
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
      def apply[X<:BaseParser with Singleton](p:X#Parser, u:UCtx[X], parent:Elt, s: Status): Element[X]
      def apply[X<:BaseParser with Singleton](p:X#Parser, u:UCtx[X], parent:Elt, s: Status, cbks: Cbks*): Element[X] with WithCallbacks
      def apply[X<:BaseParser with Singleton](p:X#Parser, u:UCtx[X], parent:Elt, s: Status, cb: Cbk, cbks: Cbks*): Element[X] with WithCallback
    }

    /** Modifies the current element behavior by using a callback
     */
    trait WithCallback extends WithCallbacks { this: Elt =>
      /** When handling Callbacks, we will want to reach the parent behaviour. */
      val cb: callbacks.Callback[Elt,Status,Ret,Key,Value] //the callback for the current element
      val cbx = cb(this)
      abstract override protected def onName(key: Key): Status            = if (cbx==null) super.onName(key)      else cbx.onName(key, super.onName)
      abstract override protected def onBeg(): Unit                       = if (cbx==null) super.onBeg()          else cbx.onBeg(super.onBeg)
      abstract override protected def onVal(v: Value): Ret                = if (cbx==null) super.onVal(v)         else cbx.onVal(v,super.onVal)
      abstract override protected def onSolver(v: Value, x: ()=>Ret): Ret = if (cbx==null) super.onSolver(v,x)    else cbx.onSolver(v, x, super.onSolver)
      abstract override protected def onEnd(): Ret                        = if (cbx==null) super.onEnd()          else cbx.onEnd(super.onEnd)
      abstract override protected def onChild(child: Elt, r: Ret): Unit   = if (cbx==null) super.onChild(child,r) else cbx.onChild(child, r, super.onChild)
    }
    /** Modifies the current element to manage a callback tree
     *  Children are built according to the following rules:
     *  - if no callback subtree applies for the child, the child returns to the base implementation, removing any overhead for callback
     *  - if a callback tree is present for the child, but no callback applies, the child uses the WithCallbacks trait
     *    this causes additional data to be carried, and some overhead when building children
     *  - if a callback tree is present for the child, and a callback applies, the child uses the WithCallback trait
     *    this causes the same overhead as the previous case; in addition, the callback is carried and base methods pass through it (onBeg etc...)
     */
    trait WithCallbacks extends EltBase { self:Elt=>
      protected[this] def cbks: Seq[Cbks] //current callbacks trees (for children)
      override def build(p: Builder#Parser, u: UCtx[Builder], s: Status): Element[Builder] = WithCallbacks(p,u,this,s,cbks,builder)
    }
    object WithCallbacks {
      /** Analyzes a callbacks sequence to know:
       *  1) whether it applies to the current item
       *  2) what sub sequence may apply to children
       */
      def apply[X<:BaseParser with Singleton](p: X#Parser, u:UCtx[X], parent:Elt, s:Status, cbks:Seq[Cbks], builder:EltBuilder): Element[X] = {
        if (cbks.length == 1) {
          //first, the case where the sequence is one element only.
          //it's very common, and should be optimized! it's furthermore much easier to read!
          cbks.head.get(s.key.toString) match {
            case None => builder(p,u,parent,s) //no subtree ? get rid of the extra callback data and associated code
            case Some(c) => c.cur match {
              case None => builder(p,u,parent,s,cbks:_*)
              case Some(cb) => builder(p,u,parent,s,cb,cbks:_*)
            }
          }
        } else {
          //that one is a little tricky; first build the next sequence of callback trees, extracted from cbks
          val r = for (x <- cbks; y <- x.get(s.key.toString)) yield y
          //if empty, return to the no callback version
          if (r.isEmpty) builder(p,u,parent,s)
          else {
            //otherwise, create the sequence of actual callbacks for this element
            val c = (for (x <- r; y <- x.cur) yield y)
            //if empty, proceed with the builder with non local callback
            if (c.isEmpty) builder(p,u,parent,s,r:_*)
            //otherwise, proceed by combining the local callbacks together to compute the final callback to apply
            else builder(p,u,parent,s,c.reduce(_(_)),r:_*)
          }
        }
      }
    }
    /** Conversion to traversable. */
    implicit def toTraversable[U](e:Elt):Traversable[Elt] = new Traversable[Elt] {
      def foreach[U](f:(Elt)=>U) = e.foreach(f)
    }
  }
  
  /** A possible implementation for the framework.
   *  It redirects the element calls to the delegate class, which contains the processor logic.
   *  This lets define standard Element classes because they now don't contain themselves the processing logic.
   */
  trait Impl extends Processor {self=>
    //the delegate type which will be used as the processor logic
    type ThisDlg = Delegate[Elt,Key,Value,Status,UCtx[BaseParser],Ret]
    //a factory for reading textual parameters
    //there will be other, specific factories
    def apply(pr: utils.ParamReader):Dlg
        
    /** Forwards the base methods to the upper layer.
     *  This causes a redirection to apply them, but usually has the immense advantage of fully defining the element by
     *  defining all behaviours. Using Motor makes it easier to define processors, all using a common element base.
     *  It also makes it possible to easily subclass an implementation.
     */
    type Dlg>:Null<:DlgBase
    trait DlgBase extends ThisDlg { this:Dlg=>
      final val proc:self.type = self
      val builder:EltBuilder
    }

    type Elt>:Null<:EltBase
    trait EltBase extends super.EltBase { self:Elt=>
      val dlg:Dlg
      def myselfImpl:dlg.proc.Elt { type Builder=self.Builder } = this
    }
    
    /** Forwards the base methods to a companion object.
     *  This causes a redirection to apply them, but usually has the immense advantage of fully defining the element by
     *  defining all behaviours. Using Delegates makes it easier to define processors, all using a common element base.
     *  It also makes it possible to easily subclass an implementation.
     *  We can already provide basic implementations for simple use case.
     */
    protected abstract class ElementBase[X<:BaseParser with Singleton] (val parser:X#Parser, val userCtx:UCtx[X], val dlg:Dlg, val key:Key, val parent:Elt) extends EltBase {this:Element[X]=>
      type Builder = X
      def builder = dlg.builder
      protected def onName(key: Key)      = dlg.onName(this,key)
      protected def onInit(): Unit        = dlg.onInit(this)
      protected def onBeg(): Unit         = dlg.onBeg(this)
      protected def onVal(v: Value): Ret  = dlg.onVal(this,v)
      protected def onEnd(): Ret          = dlg.onEnd(this)
      protected def onChild(child: Elt, r: Ret): Unit = dlg.onChild(this,child,r)      
    }    
  }
  
  /** The delegate executes the required calls for an element.
   */
  trait Delegate[-E,-K,-V,+S,+U,R] {
    protected[this] type Elt=E  //for easy method definitions
    type Result
      
    def onInit():Unit
    def onExit():Result
    // Forwarded methods
    def onName(e:E, key: K): S
    def onInit(e: E):Unit
    def onBeg(e: E): Unit
    def onVal(e: E, v: V): R
    def onEnd(e: E): R
    def onChild(e: E, child: E, r: R): Unit
  }
  //for simplicity when used on a given processor ; loses variance information, hence generality.
  type Dlg[P<:Impl] = P#Dlg
  
  /** A common pattern for defining a new Processor.
   *  It is only a guideline and doesn't have to be followed, but it helps in understanding
   *  what has to be done to write a proper processor. It also makes it easier to read a
   *  processor code which is new.
   */
  abstract class ProcessorImpl {
    //Here you fill up the spec for the implementation: mostly you close the abstract types
    trait DefImpl extends definition.Impl {
      //Here you create the delegate for the implementation
      abstract class DlgBase extends super.DlgBase {this:Dlg=>}
    }
    //Here you instantiate your processor for all modes that it supports.
    //It doesn't (and often will not) have to support all modes!
    def ctx:CtxCore with DefImpl
    def ext:ExtCore with DefImpl
    def cre:Core    with DefImpl
    //Here you provide a common way to read your specific parameters.
    protected def readParams(pr: utils.ParamReader):Product
  }
}