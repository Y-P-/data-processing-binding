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

  type Kind = String                              //produces strings
  type BaseProcessor = Def { type Parser>:Impl }  //any processor
  
  val procClass = classOf[Def]
  val kindClass = classOf[Kind]
  
  trait Parser extends Impl with utils.parsers.Handler { self=>
    final protected[this] val proc = newProc
    def location:String = proc.state.line.toString
    def err(detail: String, cause: String): Nothing = throw new IllegalStateException(cause)
    def handler: PartialFunction[Throwable,Unit] = { case e:Throwable => throw new IllegalStateException(e) }
    def push(idx: Int): Unit = push("")
    def apply(d:CharReader):Unit = proc(d)
    def read(in: java.io.Reader): Unit = this.apply(in)
    
    override def read(uri: URI, encoding: String): Unit = 
      this.apply(CharReader(ByteArrayReader(uri),if (encoding == null) "ISO-8859-15" else encoding))
  }
  abstract class AbstractParser extends Parser
}
