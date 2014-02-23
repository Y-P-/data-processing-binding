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

  type Kind = String                                              //produces strings
  type BaseProcessor = Processor { type BaseParser >: self.type } //any processor
    
  trait Impl[K,R] extends super.Impl[K,R] with utils.parsers.Handler with ParserBuilder.URLParser { this:Parser[K,R]=>
    final protected[this] val charProc = newProc
    def location:String = charProc.state.line.toString
    def err(detail: String, cause: String): Nothing = throw new IllegalStateException(cause)
    def handler: PartialFunction[Throwable,Unit] = { case e:Throwable => throw new IllegalStateException(e) }
    def push(idx: Int): Unit = push("")
    def apply(d:CharReader):Unit = charProc(d)
  }
  abstract class AbstractImpl[K,R] extends Impl[K,R] { this:Parser[K,R]=> }
}
