package parser
import parser.java._
import scala.annotation.tailrec
import _root_.java.lang.StringBuffer

/**
 * Prepares a token ring for some analysis.
 * You must call reset if you want to use the reader more than once.
 */
abstract class TokenRing(stkSz:Int,stkFw:Int,src:Source) extends TokenReader(src) {

  protected type T >: Null <: TokenElem
  protected def newToken:T
  
  protected val rb = new RingBuilder[T] {
    protected def newToken:T = TokenRing.this.newToken
  }
  
  /** loops through all token elements */
  def foreach(f:(T)=>Unit):Unit = { var t = start; do { f(t); t=t.next } while (t!=start) }  
  
  /** First element to fetch. */
  final val start = rb.build(stkSz)  
  start.toRead.pop(stkFw)
  
  /** initializes the token stack */
  override def reset = {
    super.reset
    for (t <- this) { t.kind=TokenReader.End; t.length=0; t.pos = -1; }
    start.toRead.pop(stkFw)
  }

  abstract class TokenElem extends TokenRingElem[T](this) { this:T =>
      
    /**
     * Pops n tokens from the stack and fills up the stack with as many tokens as were removed.
     * If n is 0, it advances one token forward without filling up the stack. The forward advance
     * ceases when a pop with a positive value happens.
     * This lest peek inside the stack. For exemple, suppose the stack: A B C D E
     * then pop(0) (B), pop(0) (C), pop(1) (B) and the stack becomes B C D E F
     * @param n
     * @return The next token to analyse
     */
    @tailrec final def pop(n:Int):T = if (n==0) this else { toFill.fill; next.pop(n-1) }
    
    @tailrec final def peek(n:Int):T = if (n==0) this else if (n>0) next.peek(n-1) else prev.peek(n+1)
    
    final def getStkFw = stkFw
    
    /** loops through all token elements ; the Int indicates the relative position to this token */
    def foreach(f:(T)=>Unit):Unit = {
      var i = stkFw-stkSz
      var t = peek(i)
      do { f(t); t=t.next; i+=1 } while (i<stkFw)
    }
    
    /**
     * Prints the stack as starting from this token.
     * @param here  How to mark the current place
     * @param sep   Separator between Tokens
     * @param fmt   Token formatter
     * @return
     */
    final def print(current:TokenElem, here:String, sep:String, fmt:(T)=>String) = {
      val sb = new StringBuffer
      for (t <- this) {
        if (t eq current) sb.append(here)
        sb.append(fmt(t)).append(sep)
      }
      sb.append("      near line ").append(line).toString
    }
    
    final def stackString(current:TokenElem, isLong:Boolean=false) =
      if (isLong) print(current,"<------>\n","\n",_.longString) else print(current," ^ "," ",_.baseString)

    override def toString = "["+print(this,">>",", ",_.baseString)+"]"
  }
}