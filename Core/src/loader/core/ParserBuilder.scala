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
trait ParserBuilder {selfBuilder=>

  /** Element type produced by this parser */
  type Value
  /** Key types produced by this parser */
  type Key
  //The base processor kind accepted.
  //Usually Def for generality, could be for example loader.motors.Struct.ctx.type for strong coupling.
  //Note that the processor must accept this parser implementation!
  type BaseProcessor <: Processor { type BaseParser >: selfBuilder.type }
  
  /** Actual parser implementation */
  type Parser[M<:BaseProcessor with Singleton] <: Impl[M]
  
  protected[this] type Elt[M<:BaseProcessor with Singleton] = M#Element[selfBuilder.type]
  
  /** This class glues together a parser spawned from this builder and a given processor.
   *  It requires:
   *  @param init, the function that will create the initial (top) element for the processor
   *               and a parser instance; the Launcher class in Processor provides such function.
   *  @param mapper, the function that lets translate the parser's Kind to the processor's Kind
   */
  protected[this] class Binder[-M<:BaseProcessor with Singleton](val userCtx:UserContext[selfBuilder.type,M], val init:Parser[M]=>Elt[M])
  def binder[M<:BaseProcessor with Singleton](userCtx:UserContext[selfBuilder.type,M], init:Parser[M]=>Elt[M]) = {
    new Binder[M](userCtx,init)
  }
  
  /** factory for Parser */
  def apply[M<:BaseProcessor with Singleton](bd:Binder[M]):Parser[M]
  
  /** Base implementation that merges the actual parser with the Processor.
   *  The actual implementation will mostly have to call push/pull as needed
   *  while doing its own work ; it doesn't (shouldn't) care about anything else
   *  in this trait.
   *  It has access to the launcher (hence the processor) if need be.
   *  It also has access to the user context.
   *  @param binder, which is used to attach the parser with the processor
   */
  trait Impl[M<:BaseProcessor with Singleton] extends Locator { this:Parser[M]=>
    final val builder:selfBuilder.type = selfBuilder
    val binder:Binder[M]
    protected[this] val top:Elt[M] = binder.init(this) //creating the parser associates the top element for the bound processor.
    private[this] var cur = top
    private[this] var ignore:Int = 0
    def current:Elt[M] = cur
    def userCtx = binder.userCtx
    def pull():Unit         = if (ignore>0) ignore-=1 else try { cur.pull()                     } catch errHandler finally { cur=cur.parent }
    def pull(v: Value):Unit = if (ignore>0) ignore-=1 else try { cur.pull(cur.valMap(v)) } catch errHandler finally { cur=cur.parent }
    def push(key: Key):Unit = if (ignore>0) { if (canSkip) skipToEnd else ignore+=1 } else {
      import ParserBuilder.{ skip, skipEnd }
      try { cur=cur.push(cur.keyMap(key)) } catch {
        case `skip`                 => ignore+=1
        case `skipEnd`   if canSkip => skipToEnd
        case e:Throwable if canSkip => errHandler(e); skipToEnd
        case e:Throwable            => errHandler(e); ignore+=1 
      }
    }
    def onEnd() = if (!(cur eq top)) throw new InternalLoaderException("Parsing unfinished while calling final result", null)
    def invoke(f:this.type=>Unit):M#Ret = {
      val r=top.invoke(f(this))
      onEnd()
      r
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

        
    //All exceptions handled in the same way:
    // - process exception event
    // - throw exception if and only if the exception is InternalLoaderException or UserException with lvl<=1
    def errHandler:PartialFunction[Throwable,Unit] = {
      case u:NullPointerException if current==null => try { top(StackException(u)) } finally { throw new InternalLoaderException(StackException(u),current) }
      case u:InternalLoaderException[_] => try { current(UnexpectedException(u)) } finally { throw u }
      case u:UserException              => try { current(u) } finally { if (u.lvl<=1) throw u }
      case u:Exception with Event       => try { current(u) } catch { case u:Throwable => throw new InternalLoaderException(u,current) }
      case u:Throwable                  => try { current(UnexpectedException(u)) } catch { case _:Throwable => throw new InternalLoaderException(u,current) }
    }
  }
}

object ParserBuilder {
  class SkipException extends Exception
  val skipEnd = new SkipException { override def fillInStackTrace() = this }
  val skip    = new SkipException { override def fillInStackTrace() = this }
  
  /** This provides a standard API for these parser that read Characters, such as files.
   */
  trait URLParser { this:ParserBuilder#Impl[_]=>
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
  abstract class AbstractImpl[M<:BaseProcessor with Singleton] extends super.Impl[M] { this:Parser[M]=> }
}