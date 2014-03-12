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
 *  It works extremely well with parsers that already use the push/pull mecanism...
 */
abstract class HandlerBridge extends AbstractParserBuilder {self=>
  import ParserBuilder._

  type Value = String                                             //produces string
  type Key   = String                                             //keys are string
  type BaseProcessor = Processor { type BaseParser >: self.type } //any processor
    
  trait BaseImpl extends super.BaseImpl with utils.parsers.Handler with ParserBuilder.URLParser { this:Parser=>
    final protected[this] val charProc = newProc
    def location:String = charProc.state.line.toString
    def err(detail: String, cause: String): Nothing = throw new IllegalStateException(cause)
    def handler: PartialFunction[Throwable,Unit] = { case e:Throwable => throw new IllegalStateException(e) }
    def push(idx: Int): Unit = push("")
    def apply(d:CharReader):Unit = charProc(d)
  }
  abstract class AbstractImpl extends BaseImpl { this:Parser=> }
}
