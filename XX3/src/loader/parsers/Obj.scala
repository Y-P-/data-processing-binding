package loader.parsers

import loader.core.ParserSpawner


class Obj() extends HandlerBridge {self=>

  def apply(start:BaseProcessor#Top[Kind]):Parser = new Parser(start)
  
  class Parser(val start:BaseProcessor#Top[Kind]) extends super.Parser { self=>
    override final val canSkip:Boolean = true
    override def skipToEnd():Nothing = throw new IllegalStateException //abort(0)  //this parser can skip to the end of the current data structure
    def apply(d:utils.CharReader):Unit = throw new IllegalStateException("An object parser cannot parse characters")
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