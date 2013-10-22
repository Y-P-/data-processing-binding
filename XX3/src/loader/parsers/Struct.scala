package loader.parsers

import loader.core.ParserBuilder
import loader.core.definition.Def
import loader.core.Locator
import java.net.URI
import loader.core.ParserSpawner
import utils.parsers.StructParser
import utils.parsers.State
import utils.CharReader
import utils.Reader
import loader.reflect.Converter


class Struct(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false,withComments:Boolean=false) extends Handler {self=>

  def apply(start:BaseProcessor#Top[Kind]):Parser = new Parser(start)
  
  class Parser(val start:BaseProcessor#Top[Kind]) extends StructParser('{','}','=',';','"','#',maxSz,maxDepth,nlInString,'^',Array(('t','\t'),('n','\n'),('u','+'))) with super.Parser { self=>
    override final val canSkip:Boolean = true
    override def skipToEnd():Nothing = abort(0)  //this parser can skip to the end of the current data structure
  }
}

object Struct extends ParserSpawner {
  type Parser = Struct
  def apply(pr: utils.ParamReader):Struct =
    new Struct(
        pr("maxSz", 256)(Converter.CvInt.read(null, _)),
        pr("maxDepth", 40)(Converter.CvInt.read(null, _)),
        pr("nlInString", false)(Converter.CvBoolean.read(null, _))
        )
  def apply(maxSz:Int,maxDepth:Int,nlInString:Boolean):Struct =
    new Struct(maxSz,maxDepth,nlInString)
}