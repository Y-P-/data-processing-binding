package loader.parsers

import loader.core.ParserSpawner
import utils.parsers.CharProcessor


class Obj() extends HandlerBridge {self=>

  def apply(start:BaseProcessor#Launcher[Kind]):Parser = new Parser(start)
  
  class Parser(val start:BaseProcessor#Launcher[Kind]) extends super.Parser { self=>
    def newProc:CharProcessor = null
    override final val canSkip:Boolean = true
    override def skipToEnd():Nothing = throw new IllegalStateException //abort(0)  //this parser can skip to the end of the current data structure
  }
}

object Obj extends ParserSpawner {
  import utils.StringConverter._
  type Parser = Obj
  def apply(pr: utils.ParamReader):Obj =
    new Obj(
     //   pr("maxSz", 256)(CvInt("","","10")),
     //   pr("maxDepth", 40)(CvInt("","","10")),
     //   pr("nlInString", false)(CvBoolean())
        )
  def apply():Obj =
    new Obj()
}

class PObj {
  
}