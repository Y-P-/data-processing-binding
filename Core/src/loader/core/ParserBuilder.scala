package loader.core
import java.io.Reader
import java.net.URI
import loader.core.definition.Def
import exceptions._
import loader.core.events.Event
import scala.reflect.runtime.universe._
import ParserBuilder.skip
import ParserBuilder.skipEnd
import loader.core.names.QName

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
  type Kind
  //The base processor kind accepted.
  //Usually Def for generality, could be for example loader.motors.Struct.ctx.type for strong coupling.
  //Note that the processor must accept this parser implementation!
  type BaseProcessor <: Def { type BaseParser >: selfBuilder.type }
  
  /** Actual parser implementation */
  type Parser[k] <: Impl[k]
  type Proc[k]   = BaseProcessor { type Kind=k }
  type Elt[k]    = Proc[k]#Element
  type Launch[k] = Proc[k]#Launcher
  
  /** This class glues together a given parser and a given processor.
   *  It requires:
   *  @param init, the method that will create the initial (top) element for the processor and a parser instance
   *  @param mapper, the method that lets translate the parser's Kind to the processor's Kind
   */
  abstract class Binder[K](val init:Parser[K]=>Elt[K], val mapper:(Elt[K],Kind)=>K) {
    def map(e:Elt[K],v:Kind):K = if (mapper==null) v.asInstanceOf[K] else mapper(e,v)
  }
  
  protected class Bd[K,L<:Proc[K]](val l:L) {
    class X(init:Parser[K]=>l.Element, mapper:(Elt[K],Kind)=>K) extends Binder[K](init,mapper)
    def apply(init:Parser[K]=>l.Element, mapper:(Elt[K],Kind)=>K) = new X(init,mapper)
  }
  
  /** factory for Binder */
  def binder[K](bp:Proc[K])(init:Parser[K]=>bp.Element, mapper:(Elt[K],Kind)=>K):Binder[K] = {
    val x = new Bd[K,bp.type](bp)
    new x.X(init,mapper)
  }
  
  /** factory for Parser */
  def apply[K](bd:Binder[K]):Parser[K]
  
  /** Base implementation that merges the actual parser with the Processor.
   *  The actual implementation will mostly have to call push/pull as needed
   *  while doing its own work ; it doesn't (shouldn't) care about anything else
   *  in this trait.
   *  It has access to the launcher (hence the processor) if need be.
   *  It also has access to the user context.
   *  @param launcher, the launcher used for the processor
   *  @param exec, the executor (launcher.X) for the launcher
   */
  trait Impl[K] extends Locator { this:Parser[K]=>
    final val builder:selfBuilder.type = selfBuilder
    val binder:Binder[K]
    val top:Elt[K] = binder.init(this) //creating the parser associates the top element for the bound processor.
    private[this] var cur = top
    private[this] var ignore:Int = 0
    def current = cur
    def userCtx = top.userCtx
    def pull():Unit        = if (ignore>0) ignore-=1 else try { cur.pull()                  } catch errHandler finally { cur=cur.parent }
    def pull(v: Kind):Unit = if (ignore>0) ignore-=1 else try { cur.pull(binder.map(cur,v)) } catch errHandler finally { cur=cur.parent }
    def push(name: String):Unit = if (ignore>0) { if (canSkip) skipToEnd else ignore+=1 } else {
      import ParserBuilder.{ skip, skipEnd }
      try { cur=cur.push(name) } catch {
        case `skip`                 => ignore+=1
        case `skipEnd`   if canSkip => skipToEnd
        case e:Throwable if canSkip => errHandler(e); skipToEnd
        case e:Throwable            => errHandler(e); ignore+=1 
      }
    }
    def onEnd() = if (!(cur eq top)) throw new InternalLoaderException("Parsing unfinished while calling final result", null)
    /** Read data from an URI.
     */
    def read(uri:URI,encoding:String):Unit = {
      val e = if (encoding==null) "ISO-8859-15" else encoding
      val in = new java.io.InputStreamReader(uri.toURL.openStream,e)
      try { read(in) } finally { in.close }
    }
    /** Read data from an input stream.
     */
    def read(in: Reader): Unit
    
    /** when the processor rejects the current elemnt (push returns false), the parser is assumed to get to the
     *  next element at the same level, ignoring the current element and sub-structure.
     *  return false if this is not possible (this is always safe.)
     */
    def canSkip: Boolean = false  //a safe value
    /** do this to skip to end of current structure. This happens when the processor doesn't need more data.
     *  do nothing if not possible.
     */
    def skipToEnd(): Unit = ()
    /** build a QName from a string input.
     *  The default returns null and is handled as a QName that is not an attribute and has no namespace.
     */
    def qName(s:String):QName = new QName(s)

        
    //All exceptions handled in the same way:
    // - process exception event
    // - throw exception if and only if the exception is InternalLoaderException or UserException with lvl<=1
    def errHandler:PartialFunction[Throwable,Unit] = {
      case u:InternalLoaderException => try { current(UnexpectedException(u)) } finally { throw u }
      case u:UserException           => try { current(u) } finally { if (u.lvl<=1) throw u }
      case u:Exception with Event    => try { current(u) } catch { case u:Throwable => throw new InternalLoaderException(u,current) }
      case u:Throwable               => try { current(UnexpectedException(u)) } catch { case _:Throwable => throw new InternalLoaderException(u,current) }
    }
  }
}

object ParserBuilder {
  class SkipException extends Exception
  val skipEnd = new SkipException { override def fillInStackTrace() = this }
  val skip    = new SkipException { override def fillInStackTrace() = this }
}

abstract class AbstractParserBuilder extends ParserBuilder {
  abstract class AbstractImpl[K] extends super.Impl[K] { this:Parser[K]=> }
}