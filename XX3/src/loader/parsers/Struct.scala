package loader.parsers

import loader.core.ParserSpawner
import utils.parsers.StructParser
import loader.core.UsrCtx


class Struct(maxSz:Int=256,maxDepth:Int=32,nlInString:Boolean=false) extends HandlerBridge {self=>
    
  def apply[P<:BaseProcessor with Singleton](userCtx:UCtx[P],pf:Impl[P]=>P#Elt):Impl[P] = new Impl(userCtx,pf)
  
  type Parser = BaseImpl
  class Impl[X<:BaseProcessor with Singleton](val userCtx:UCtx[X],pf: Impl[X]=>X#Elt) extends StructParser('{','}','=',';','"','#',maxSz,maxDepth,nlInString,'^',Array(('t','\t'),('n','\n'),('u','+'))) with super.BaseImpl with Efficient {
    lazy val top = pf(this)                      //must be lazy. If not, it is initialized too late. It cannot be initialized upwards without a cast (because only here is 'this' a Impl[X]).
    type Proc = X
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