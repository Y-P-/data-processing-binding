package parser.px
import parser.TokenRing
import parser.Source
import parser.px.Data.{bClass,bString}
import scala.annotation.{tailrec,switch}


/**
 * An actual parser for PX tokens.
 * It is heavily optimized for performances : each line, each construct has been thouroughly reviewed,
 * including bytecode, tests on the JVM etc. Usually, several variants were used.
 * This will correctly parse any text in ASCII, ISO-8859-X, or UTF-8 character set.
 * In that way, it is considered sufficient, especially considering that the processing doesn't
 * require internal transcoding to UTF-16 (including the time consuming conversion); that transcoding
 * may still be required on demand by the client code, but it will only be done on the specific parts
 * of the data, not the whole file. This may be detrimental to the global performance on a large file
 * where you keep many String unless you use a CharsetDecoder.
 */
class Tokenizer(val src:Source) extends TokenRing(8,5,src) {
  import Tokenizer._
  
  protected type T = Token
  protected final def newToken = new Token

  class Token extends TokenElem {
    final var next2:Token = _
  
    /**
     * Fills last with the next token.
     * Accounts for position, newline and struct level, removes comments and spaces
     * End of flow results in an exception
     */
    @inline @tailrec protected final def doParse(p:Int):Int = {
      var q = p+1
      var k = bClass(data(q))
      (k: @switch) match {  //note that the case values are constants from Tokenizer
        case `{` => kind=Open;  pos=q; length=1; q
        case `}` => kind=Close; pos=q; length=1; q
        case `=` => kind=Equal; pos=q; length=1; q
        case `"` => readStr(q)
        case `#` => do { q+=1 } while(data(q)!=10); doParse(q-1)
        case `n` => line+=1; doParse(q)
        case ` ` => do { q+=1 } while (bClass(data(q))==` `); doParse(q-1)
        case `c` => pos=q; do { q+=1 } while (bClass(data(q))==`c`); kind=Data; length=q-pos; q-1
      }
    }
    @inline private final def readStr(q0:Int):Int = {
      var q=q0; pos=q0
      do {
        q+=1
        val k = data(q) match {
          case x if x<0 => 0
          case x        => bString(x)
        }
        (k: @switch) match {
          case 0 => //continue
          case 1 => kind=Error; pos=q0; length=0; return q  //unexpected EOL
          case 2 => if (data(q-1)!='\\') { kind=Data; length=q+1-pos; return q } //character ", possibly escaped
        }
      } while (true)
      0  //never reached
    }
    final def fill:Unit = {
      try {
        position=doParse(position)
      } catch {
        case _:Throwable => kind=End
      }
      next
    }
    override def bind = { super.bind; next2=next.next }

  }

  /** printable name for tokens */
  final def tokenName(kind:Int) = Tokenizer.tokenName(kind)
}


final object Tokenizer {
 
  //Token class
  final val Error  = parser.java.TokenReader.Error
  final val End    = parser.java.TokenReader.End
  final val Open   = 0
  final val Close  = 1
  final val Equal  = 2
  final val Data   = 3
  
  final def tokenName(kind:Int) = kind match {
      case Error => "Error"
      case End   => "End"
      case Open  => "Open"
      case Close => "Close"
      case Equal => "Equal"
      case Data  => "Data"
      case _     => "???"
    }
      
  final val `"` = 5 //:Byte = 34
  final val `{` = 6 //:Byte = 123
  final val `}` = 7 //:Byte = 125
  final val `=` = 4 //:Byte = 61
  final val `n` = 3 //:Byte = 10
  final val `#` = 2 //:Byte = 35
  final val ` ` = 0 //:Byte = 32
  final val `c` = 1 // char

}
