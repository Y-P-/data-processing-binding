package loader.parsers

import java.net.URI
import utils.parsers.State
import utils.{CharReader,ByteArrayReader}
import loader.core.{AbstractParserBuilder,ParserBuilder,ParserSpawner,Locator}
import loader.core.definition.Processor

/** A standard bridge between utils.parsers.Handler and ParserBuilder.Parser.
 *  You only have to :
 *  - call push/pull as needed (provided by Handler)
 *  - care for the behavior with abort
 */
abstract class HandlerBridge extends AbstractParserBuilder {self=>
  import ParserBuilder._

  type Value = String                                             //produces string
  type Key   = String                                             //keys are string
  type BaseProcessor = Processor { type BaseParser >: self.type } //any processor
    
  trait Impl[K,V,R] extends super.Impl[K,V,R] with utils.parsers.Handler with ParserBuilder.URLParser { this:Parser[K,V,R]=>
    final protected[this] val charProc = newProc
    def location:String = charProc.state.line.toString
    def err(detail: String, cause: String): Nothing = throw new IllegalStateException(cause)
    def handler: PartialFunction[Throwable,Unit] = { case e:Throwable => throw new IllegalStateException(e) }
    def push(idx: Int): Unit = push("")
    def apply(d:CharReader):Unit = charProc(d)
  }
  abstract class AbstractImpl[K,V,R] extends Impl[K,V,R] { this:Parser[K,V,R]=> }
}
