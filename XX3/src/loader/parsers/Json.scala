package loader.parsers

import loader.core.ParserSpawner
import utils.parsers.JsonParser


class Json(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false,withComments:Boolean=false) extends HandlerBridge {self=>

  def apply(start:BaseProcessor#Launcher[Kind]):Parser = new Parser(start)
  
  class Parser(val start:BaseProcessor#Launcher[Kind]) extends JsonParser(maxSz,maxDepth,nlInString,withComments) with super.Parser { self=>
    override final val canSkip = true
    override def skipToEnd():Nothing = abort(0)  //this parser can skip to the end of the current data structure
  }
}

object Json extends ParserSpawner {
  import utils.StringConverter._
  type Parser = Json
  def apply(pr: utils.ParamReader):Json       =
    new Json(
        pr("maxSz", 256)(CvInt("","",null)),
        pr("maxDepth", 40)(CvInt("","",null)),
        pr("nlInString", false)(CvBoolean()),
        pr("withComments", false)(CvBoolean())
        )
  def apply(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false,withComments:Boolean=false):Json =
    new Json(maxSz,maxDepth,nlInString,withComments)  
}