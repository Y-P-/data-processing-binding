package loader.core
import java.io.Reader
import java.net.URI
import loader.core.definition.Processor
import exceptions._
import loader.core.events.Event
import scala.reflect.runtime.universe._
import ParserBuilder.skip
import ParserBuilder.skipEnd

trait Locator {
  /** A string that explicits where the parser is */
  def location:String
}

/**
 * Implements the parser side of the pair parser/motor.
 * It receives parser events from the actual parser (push/pull), and translates them into
 * motor events (onName0/onEnd0.) While the actual parser works on a flat level (all data
 * it receives is equal), this class also gives depth by creating a new Elt every time a
 * push is received. Elt are a stack, and push events remove the last Elt from the stack.
 * Thus, the processor side has a vision in depth of the data structure.
 * 
 * A given implementation should respect the following guideline:
 * o Do not expect the initial element onBeg method to have been called
 *   But do not call it in the parser : this would cause problems later in includes
 * o Do not invoke the initial element onEnd method
 * This means that the top element onBeg/onEnd methods must be called by the user, if necessary.
 * 
 */
trait ParserBuilder {

  /** Element type produced by this parser */
  type Value
  /** Key types produced by this parser */
  type Key
  /** A return value for the parser */
  type Ret
  //The base processor kind accepted.
  //Usually Def for generality, could be for example loader.motors.Struct.ctx.type for strong coupling.
  //Note that the processor must accept this parser implementation!
  type BaseProcessor>:Null<:Processor
  //the abstract types which must be filled up by each implementation
  type UCtx[-P<:BaseProcessor]>:Null<:UsrCtx[this.type,P]                       //the kind of UserCtx used
  protected type Impl[X<:BaseProcessor with Singleton]<:Parser { type Proc=X }  //the concrete implementation
  type Parser>:Null<:BaseImpl                                                   //the concrete implementation
  
  //used for dynamic checks
  def baseProcessorClass:Class[BaseProcessor]
  def baseUCtxClass:Class[UCtx[_]]
    
  /** factory for Parser */
  def apply[X<:BaseProcessor with Singleton](u:UCtx[X],pf:Impl[X]=>X#Elt):Impl[X]
  
  
  /** This implementation does a cast.
   *  It is has the advantage of:
   *  - building/sharing only one instance of EltCtx for one element
   *  But it cannot ensure type security unless the user context passed on to the processor and parsers
   *  are identical and that this type respects: UCtx[Proc] with Proc#UCtx[ParserBuilder.this.type]
   *  In effect, this cannot be imposed here without falling into the 'volatile type' trap.
   *  It will have to be ensured at the calling level.
   *  IMPORTANT: object 'run' methods ensure this condition.
   */
  trait Efficient {this:BaseImpl=>
    def eltCtx:ECtx[ParserBuilder.this.type,Proc] = cur.eltCtx.asInstanceOf[ECtx[ParserBuilder.this.type,Proc]]
  }
  /** This implementation is type secure.
   *  But! eltCtx is recomputed every time. This may not be important (simple implementations will return a
   *  common object) ; but this can be annoying when specific instances are built. In particular, this forces
   *  these instances to be fully functional (no state would get shared.) Furthermore, this can severely impair
   *  performances when computing eltCtx is costly.
   */
  trait Sure {this:BaseImpl=>
    def eltCtx:ECtx[ParserBuilder.this.type,Proc] = userCtx(cur)
  }
  
  /** Base implementation that merges the actual parser with the Processor.
   *  The actual implementation will mostly have to call push/pull as needed
   *  while doing its own work ; it doesn't (shouldn't) care about anything else
   *  in this trait.
   */
  trait BaseImpl extends Locator {
    type Proc <: BaseProcessor with Singleton
    val userCtx:UCtx[Proc]
    val top:Proc#Elt
    def eltCtx:ECtx[ParserBuilder.this.type,Proc]
    protected[this] var cur = top
    private[this] var ignore:Int = 0
    def current = cur
    def pull():Unit         = if (ignore>0) ignore-=1 else try { cur.pull() } catch errHandler finally { cur=cur.parent }
    def pull(v: Value):Unit = if (ignore>0) ignore-=1 else try { cur.pull(eltCtx.valMap(v)) } catch errHandler finally { cur=cur.parent }
    def push(key: Key):Unit = if (ignore>0) { if (canSkip) skipToEnd else ignore+=1 } else {
      import ParserBuilder.{ skip, skipEnd }
      try { cur=cur.push(eltCtx.keyMap(key)) } catch {
        case `skip`                 => ignore+=1
        case `skipEnd`   if canSkip => skipToEnd
        case e:Throwable if canSkip => errHandler(e); skipToEnd
        case e:Throwable            => errHandler(e); ignore+=1 
      }
    }
    def onExit():Ret
    def invoke(f:this.type=>Unit):(Ret,Proc#Ret) = {
      if (!(top.userCtx eq userCtx)) throw new InternalLoaderException("userCtx on processor and parser are unconsistent", null)
      val r=top.invoke(f(this))
      if (!(cur eq top)) throw new InternalLoaderException("Parsing unfinished while calling final result", null)
      (onExit(),r)
    }
    def errHandler = try {
      eltCtx.errHandler(this)
    } catch {  //only NullPointerException expected here, but who knows ?
      case u:Throwable => top(StackException(u)); throw u
    }
    
    /** when the processor rejects the current elemnt (push returns false), the parser is assumed to get to the
     *  next element at the same level, ignoring the current element and sub-structure.
     *  return false if this is not possible (this is always safe.)
     */
    def canSkip: Boolean = false  //a safe value
    /** do this to skip to end of current structure. This happens when the processor doesn't need more data.
     *  do nothing if not possible.
     */
    def skipToEnd(): Unit = ()
  }
}

object ParserBuilder {
  class SkipException extends Exception
  val skipEnd = new SkipException { override def fillInStackTrace() = this }
  val skip    = new SkipException { override def fillInStackTrace() = this }
  
  /** This provides a standard API for these parser that read Characters, such as files.
   */
  trait URLParser { this:ParserBuilder#BaseImpl=>
    def defaultEncoding = "ISO-8859-15"

    /** Reader from a CharReader. It is usually sufficient to fill this up. */
    def apply(d:utils.CharReader):Unit
      
    /** Read data from an URL. */
    @throws(classOf[java.io.IOException])
    def read(url:java.net.URL,encoding:String):Unit = {
      val e = if (encoding==null) defaultEncoding else encoding
      val in = new java.io.InputStreamReader(url.openStream,e)
      try { read(in) } finally { in.close }
    }
    
    /** Read data from an URL with prefetch in an array. */
    @throws(classOf[java.io.IOException])
    def readPrefetch(url:java.net.URL,encoding:String):Unit = {
      val e = if (encoding==null) defaultEncoding else encoding
      apply(utils.CharReader(utils.ByteArrayReader(url),e))
    }
    
    /** Read data from an input stream. */
    @throws(classOf[java.io.IOException])
    def read(in: Reader): Unit = apply(in)
  }
}

abstract class AbstractParserBuilder extends ParserBuilder {
  //provides shared classes to work on when possible: reduce code volume.
}