package loader.core
import java.io.Reader
import java.net.URI
import loader.core.definition.Def
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
  import ParserBuilder._

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
  
  /** tells if the parser natively supports attributes ; if not, we will rely on a naming scheme */
  protected val nativeAttribs = false
  
  trait Impl extends Locator {
    def parsClass = getClass
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
    val top = start.init(this)
    private var cur:top.Element = top
    private var ignore:Int = 0
    def current = cur
    def userCtx = top.userCtx
    def pull():Unit        = if (ignore>0) ignore-=1 else try { cur.pull()  } catch { errHandler } finally { cur=cur.parent }
    def pull(v: Kind):Unit = if (ignore>0) ignore-=1 else try { cur.pull(start.map(cur,v)) } catch { errHandler } finally { cur=cur.parent }
    def push(name: String): Boolean = if (ignore>0) true else {
      import ParserBuilder.{ skip, skipEnd }
      try { cur=cur.push(name); true } catch {
        case `skip`      if ignoring  => false
        case `skip`                   => ignore+=1; true
        case `skipEnd`   if ignoring  => skipping; false
        case `skipEnd`                => ignore+=1; true
        case e:Throwable if ignoring  => errHandler(e); false 
        case e:Throwable              => ignore+=1; errHandler(e); true 
      }
    }
    def invoke(f: => Unit) = {
      val r=cur.invoke(f)
      if (!(cur eq top)) throw new InternalLoaderException("Parsing unfinished while calling final result", null)
      r
    }
    //XXX final protected val isAttr:(String)=>Boolean = if (!nativeAttribs && features.isAttr!=null) (s:String)=>features.isAttr.matcher(s).matches else null
    //XXX final protected[this] val subst:(Kind)=>Kind = if (userCtx.vars==null) null else features.substBuilder(userCtx.vars)(cur,_) //variable substitution call
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
    def ignoring: Boolean = false  //a safe value (albeit slow parser)
    /** do this to skip to end of current structure. This happens when the processor doesn't need more data.
     *  do nothing if not possible.
     */
    def skipping(): Unit = ()
        
    //All exceptions handled in the same way:
    // - process exception event
    // - throw exception if and only if the exception is InternalLoaderException or UserException with lvl<=1
    def errHandler:PartialFunction[Throwable,Unit] = {
      case u:InternalLoaderException => try { cur(UnexpectedException(u)) } finally { throw u }
      case u:UserException           => try { cur(u) } finally { if (u.lvl<=1) throw u }
      case u:Exception with Event    => try { cur(u) } catch { case u:Throwable => throw new InternalLoaderException(u,cur) }
      case u:Throwable               => try { cur(UnexpectedException(u)) } catch { case _:Throwable => throw new InternalLoaderException(u,cur) }
    }
  }
}

object ParserBuilder {
  import scala.language.existentials
  class SkipException extends Exception
  val skipEnd = new SkipException { override def fillInStackTrace() = this }
  val skip    = new SkipException { override def fillInStackTrace() = this }
}