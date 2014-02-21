package loader.parsers

import loader.core.ParserSpawner
import utils.parsers.StructParser


class Struct(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false) extends HandlerBridge {self=>
    
  def apply[K,R](b:Binder[K,R]):Parser[K,R] = new Parser(b)
  
  class Parser[K,R](val binder:Binder[K,R]) extends StructParser('{','}','=',';','"','#',maxSz,maxDepth,nlInString,'^',Array(('t','\t'),('n','\n'),('u','+'))) with super.Impl[K,R] {
    override final val canSkip:Boolean = true
    override def skipToEnd():Nothing = abort(0)  //this parser can skip to the end of the current data structure
  }
}

object Struct extends ParserSpawner {
  import utils.StringConverter._
  type Parser = Struct
  def apply(pr: utils.ParamReader):Struct =
    new Struct(
        pr("maxSz", 256)(CvInt("","","10")),
        pr("maxDepth", 40)(CvInt("","","10")),
        pr("nlInString", false)(CvBoolean())
        )
  def apply(maxSz:Int,maxDepth:Int,nlInString:Boolean):Struct =
    new Struct(maxSz,maxDepth,nlInString)
}