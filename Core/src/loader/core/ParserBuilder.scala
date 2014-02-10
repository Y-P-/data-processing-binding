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
trait ParserBuilder {

  /** Element type produced by this parser */
  type Kind
  //The base processor kind accepted.
  //Usually Def for generality, could be for exemple loader.motors.Struct.ctx.type for strong coupling.
  //Note that the processor must accept this parser implementation!
  type BaseProcessor <: Def { type Parser>:Impl }
  
  def procClass:Class[_<:Def]
  def kindClass:Class[Kind]
    
  /** Loading from a Reader. */
  def run(in: Reader) = exec((x:Impl)=>x.read(in))
  /** Loading from an uri with encoding possibly specified. */
  def run(uri: URI, encoding: String) = exec(_.read(uri, encoding))
  /** running with that element builder as root */
  def apply(start:BaseProcessor#Top[Kind]):Impl
  /** executor for a specific invoker */
  def exec(f: Impl=>Unit) = new Executor(f)
  
  /**
   * A class that contains all information required to run a parser, except the builder used.
   *  It is most notably handy for turning the parser into an iterable or using xpath.
   *  You should normally not have to subclass this.
   */
  class Executor(f: Impl=>Unit) {
    import scala.language.existentials
    val builder:ParserBuilder.this.type = ParserBuilder.this
    final def apply(start:builder.BaseProcessor#Top[builder.Kind]) = {
      val r = ParserBuilder.this.apply(start)
      r.invoke(f(r))
    }
  }
  
  trait Impl extends Locator {
    def parsClass:Class[_<:Impl] = getClass
    val start:BaseProcessor#Top[Kind]
    //dynamic check to ensure that the parser can run with the processor.
    //usually, the compile time check is sufficient, but not in case of includes.
    // 1) Check parser is acceptable for processor
    if (parsClass!=null && start.processor.parsClass!=null && !(start.processor.parsClass.isAssignableFrom(parsClass)))
      throw new IncludeException(1,parsClass,start.processor.procClass)
    // 2) Check processor is acceptable for parser
    if (procClass!=null && start.processor.procClass!=null && !(procClass.isAssignableFrom(start.processor.procClass)))
      throw new IncludeException(2,start.processor.procClass,parsClass)
    // 3) Check parser kind is compatible with Top kind
    start.check(ParserBuilder.this)
    
    final val builder:ParserBuilder.this.type = ParserBuilder.this
    val top:start.processor.Element = start.init(this)
    protected[this] var cur:top.Element = top
    protected[this] var ignore:Int = 0
    def current:top.Element = cur
    def userCtx = top.userCtx
    def pull():Unit        = if (ignore>0) ignore-=1 else try { cur.pull()  } catch errHandler finally { cur=cur.parent }
    def pull(v: Kind):Unit = if (ignore>0) ignore-=1 else try { cur.pull(start.map(cur,v)) } catch errHandler finally { cur=cur.parent }
    def push(name: String):Unit = if (ignore>0) { if (canSkip) skipToEnd else ignore+=1 } else {
      import ParserBuilder.{ skip, skipEnd }
      try { cur=cur.push(name) } catch {
        case `skip`                 => ignore+=1
        case `skipEnd`   if canSkip => skipToEnd
        case e:Throwable if canSkip => errHandler(e); skipToEnd
        case e:Throwable            => errHandler(e); ignore+=1 
      }
    }
    def invoke(f: => Unit):top.Ret = {
      val r=cur.invoke(f)
      if (!(cur eq top)) throw new InternalLoaderException("Parsing unfinished while calling final result", null)
      r
    }
    /** Read data from an URI.
     */
    def read(uri:URI,encoding:String):Unit = {
      val e = if (encoding==null) "ISO-8859-15" else encoding
      val in = new java.io.InputStreamReader(uri.toURL.openStream,e)
      try { read(in) } finally { in.close }
    }
    /** Read data from an inputstream.
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
  
  /** define useful generics */
  protected[this] type P0[-x] = ParserBuilder { type Kind>:x }
  protected[this] type P1[+x] = ParserBuilder { type Kind<:x }
  type Impl[-x] = P0[x]#Impl
  type Exc[+x]  = P1[x]#Executor
}

abstract class AbstractParserBuilder extends ParserBuilder {
  abstract class Impl extends super.Impl
}