package loader.parsers

import java.net.URI
import utils.parsers.State
import utils.{CharReader,ByteArrayReader}
import loader.core.{AbstractParserBuilder,ParserBuilder,ParserSpawner,Locator}
import loader.core.definition.Def

/** A standard bridge between utils.parsers.Handler and ParserBuilder.Parser.
 *  You should care about :
 *  - def apply(d:CharReader):Unit  (statring the underlying parser)
 *  - behaviour with abort
 */
abstract class HandlerBridge extends AbstractParserBuilder {self=>
  import ParserBuilder._

  type Kind = String                                        //produces strings
  type BaseProcessor = Def { type BaseParser >: self.type } //any processor
    
  trait Impl[K] extends super.Impl[K] with utils.parsers.Handler { this:Parser[K]=>
    final protected[this] val charProc = newProc
    def location:String = charProc.state.line.toString
    def err(detail: String, cause: String): Nothing = throw new IllegalStateException(cause)
    def handler: PartialFunction[Throwable,Unit] = { case e:Throwable => throw new IllegalStateException(e) }
    def push(idx: Int): Unit = push("")
    def apply(d:CharReader):Unit = charProc(d)
    def read(in: java.io.Reader): Unit = this.apply(in)
    
    override def read(uri: URI, encoding: String): Unit = 
      this.apply(CharReader(ByteArrayReader(uri),if (encoding == null) "ISO-8859-15" else encoding))
  }
  abstract class AbstractImpl[K] extends Impl[K] { this:Parser[K]=> }
}
