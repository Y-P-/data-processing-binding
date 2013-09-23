package loader.parsers

import java.io.Reader
import java.net.URI
import parser.Source
import parser.px.analyzers.SimpleAnalyzer
import parser.px.{ Analyzer, AnalyzerAbortException }
import loader._
import loader.core._
import loader.core.definition.Def
import loader.reflect.Converter

class Struct(val keepQuotes: Boolean = false) extends ParserBuilder {self=>
  import ParserBuilder._

  type Kind = String                              //produces strings
  type BaseProcessor = Def { type Parser>:Impl }  //any processor
  
  val procClass = classOf[Def]
  val kindClass = classOf[Kind]

  def apply(start:BaseProcessor#Top[Kind]):Parser = new Parser(start)

  class Parser(val start:BaseProcessor#Top[Kind]) extends Impl with Locator with Analyzer with SimpleAnalyzer { self=>
    type R = Unit
    final def line = if (tokenizer != null) tokenizer.line else -1
    def location = line.toString
    private final def str(v: Tk) = if (!keepQuotes) v.unquotedString else v.baseString
    protected final def ignore(name: Tk) = !push(name.baseString)
    protected final def closeList() = pull
    protected final def closeStruct() = pull
    protected final def closeArray() = pull
    protected final def appendList(value: Tk) = { push(""); pull(str(value)) }
    protected final def putStruct(name: Tk) = ()
    protected final def putAnonStruct() = push("")
    protected final def putField(name: Tk, data: Tk) = pull(str(data))
    protected final def putList(name: Tk) = ()
    protected final def putEmpty(name: Tk) = pull
    protected final def putArray(name: Tk) = ()
    protected final def end = ()
    override def skipping() = throw new AnalyzerAbortException(0, 0)  //this parser can skip to the end of the current data structure
    override val ignoring = true                                      //this parser can skip over unwanted data structures

    def read(in: Reader) = throw new UnsupportedOperationException
    override def read(uri: URI, encoding: String): Unit = {
      val e = if (encoding == null) "ISO-8859-15" else encoding
      if (!Struct.charset.contains(e)) throw new IllegalArgumentException(s"unsupported charset ${e}")
      apply(new Source(uri, e))
    }
  }
}

object Struct extends ParserSpawner {
  type Parser = Struct
  def apply(pr: utils.ParamReader):Struct       = new Struct(pr("keepQuotes", false)(Converter.CvBoolean.read(null, _)))
  def apply(keepQuotes: Boolean = false):Struct = new Struct(keepQuotes)
  
  final val charset = Set("US-ASCII", "ISO-8859-15", "ISO-8859-14", "ISO-8859-13", "ISO-8859-11", "ISO-8859-10",
    "ISO-8859-9", "ISO-8859-8", "ISO-8859-7", "ISO-8859-6", "ISO-8859-5", "ISO-8859-4",
    "ISO-8859-3", "ISO-8859-2", "ISO-8859-1", "UTF-8")
  
}
