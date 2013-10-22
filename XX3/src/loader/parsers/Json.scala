package loader.parsers

import loader.core.ParserBuilder
import loader.core.definition.Def
import loader.core.Locator
import java.net.URI
import loader.core.ParserSpawner
import utils.parsers.JsonParser
import utils.parsers.State
import utils.CharReader
import utils.Reader
import loader.reflect.Converter


class Json(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false,withComments:Boolean=false) extends Handler {self=>

  def apply(start:BaseProcessor#Top[Kind]):Parser = new Parser(start)
  
  class Parser(val start:BaseProcessor#Top[Kind]) extends JsonParser(maxSz,maxDepth,nlInString,withComments) with super.Parser { self=>
    override final val canSkip = true
    override def skipToEnd():Nothing = abort(0)  //this parser can skip to the end of the current data structure
  }
}

object Json extends ParserSpawner {
  type Parser = Json
  def apply(pr: utils.ParamReader):Json       =
    new Json(
        pr("maxSz", 256)(Converter.CvInt.read(null, _)),
        pr("maxDepth", 40)(Converter.CvInt.read(null, _)),
        pr("nlInString", false)(Converter.CvBoolean.read(null, _)),
        pr("withComments", false)(Converter.CvBoolean.read(null, _))
        )
  def apply(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false,withComments:Boolean=false):Json =
    new Json(maxSz,maxDepth,nlInString,withComments)  
}