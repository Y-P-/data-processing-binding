package parser.px

import scala.annotation.switch
import parser.Source
  
/**
 * Internal Exception used to differentiate between client code exceptions
 * and Analyzer exceptions. The first indicate client code error. The second
 * likely indicate parsing error. If no differentiation were done, client code
 * errors would appera as parsing errors.
 */
protected case class AnalyzerException(e:Throwable,msg:String="") extends Exception(msg,e) {
  override def canEqual(x:Any) = x.isInstanceOf[AnalyzerException]
  override def fillInStackTrace:Throwable = { this }
}
protected case class NonAnalyzerException(e:Throwable) extends Exception {
  override def canEqual(x:Any) = x.isInstanceOf[AnalyzerException]
  override def fillInStackTrace:Throwable = { this }
}

/**
 * An exception used to abort the analysis of the current structure and possibly
 * container structures. This is useful to prevent useless work that's going to
 * be discarded because some condition was not met.
 * Note that this exception must not be raised in any close... call ; if it is
 * raised there, it will abort the analysis.
 * The difference between ignore and this exception is that ignore will escape
 * the next coming frame, whereas this exception escapes from the current frame.
 * In addition, ignore only works on name patterns, while throwing an exception
 * can be done on any kind of conditions.
 * @param lvl     number of frames to drop (1=current, 2=current+containing etc.)
 * @param discard number of frames to discard (call abort instead of close)
 *                when frames are discarded, they start from the top
 *                lvl counts the discarded frames (i.e. lvl >= discard)
 */
case class AnalyzerAbortException(lvl:Int,discard:Int=0) extends Exception {
  if (lvl<0) throw new IllegalArgumentException("Abort exception parameter must be positive or nul")
  override def canEqual(x:Any) = x.isInstanceOf[AnalyzerAbortException]
  override def fillInStackTrace:Throwable = this
}

trait Analyzer {
  
  protected type Tk = Tokenizer#Token
  protected type R
  
  /**
   * Exception thrown when the analyzer could end it's work but the flow is not empty.
   * An unexpected closing brace is found. This may or may not be an error.
   * Furthermore, the analyzer has a result!
   */
  case class PartialAnalyzisException(r:R) extends Exception {
    override def canEqual(x:Any) = x.isInstanceOf[PartialAnalyzisException]
    override def fillInStackTrace:Throwable = { this }
  }

  /**
   * Methods to override to do anything.
   * These are the full methods, including open/close positions.
   */
  protected def ignore(name:Tk):Boolean
  protected def closeList(close:Int):Unit
  protected def closeStruct(close:Int):Unit
  protected def closeArray(close:Int):Unit
  protected def abortList(close:Int):Unit
  protected def abortStruct(close:Int):Unit
  protected def abortArray(close:Int):Unit
  protected def appendList(value:Tk):Unit
  protected def putStruct(name:Tk,open:Int):Unit
  protected def putAnonStruct(open:Int):Unit
  protected def putField(name:Tk,open:Tk):Unit
  protected def putList(name:Tk,open:Int):Unit
  protected def putEmpty(name:Tk,open:Int,close:Int):Unit
  protected def putArray(name:Tk,open:Int):Unit
  protected def end:R
  
  /** The current depth in the structures tree ; can be read by the previous methods. */
  final protected var depth = 0
  /** The current tokenizer ; can be read by the previous methods. */
  final protected var tokenizer:Tokenizer = null
    
  @inline protected def handler:PartialFunction[Throwable,Nothing] = { //common exception handler for user defined calls
    case e:AnalyzerAbortException => throw e
    case e                        => throw new AnalyzerException(e,"")
  }
  @inline protected def err(tk:Tk) = throw new IllegalStateException("Unexpected Token: "+tk.longString)
    
  /*
   * Table of transitions.
   * C : Close
   * E : Equal
   * O : Open
   * D : Data
   * e : end
   * 
   * First column  : element read (= current token, or 0 as refered below)
   * Second column : action (none if no action)
   * Third column  : next state
   * Fourth column : lookahead advance
   * Fifth column  : number of tokens consummed
   * Sixth column  : stack state
   * 
   * We use the Tokenizer, which has a lookahead, and we can refer to next
   * tokens by using positive numbers.
   * 0 : first usued token
   * etc...
   * 
   * In a given state, we must find one of the listed tokens. If not, we
   * are in error.
   * 
   * The state begins at 0.
   * 
   * 0) D    none           1   0  0  D     // start field
   *    e    terminate                e     // eof, error if l>0
   *    C    closestruct    0   0  1  }     // let it generate an exception if no open struct
   * 1) E    none           2   1  0  D= 
   * 2) O    none           3   2  0  D={   // begin struct or list
   *    D    data(0,2)      0   2  3  D=D   // found simple field
   * 3) C    empty(0)       0   3  4  D={}  // found empty struct/list
   *    D    none           4   3  0  D={D  // data disambiguation
   *    O    anonlist(0)    6   3  3  D={{  // anonymous list 
   * 4) D    list(0)        5   4  3  D={DD // found list
   *    C    list(0)        5   4  3  D={D} // found one elt list
   *    E    struct(0)      0   4  3  D={D= // found struct
   * 5) D    appendlist(0)  5   0  1  D     // found element in list
   *    C    closelist      0   0  1  }     // found end of list
   *    
   * 6) O    openanon       7   0  1  {     // start of anon
   * 7) D    none           8   0  0  D     // start of anon field
   *    C    closeanon     10   0  1  }     // end of anon
   * 8) =    none           9   1  0  D=    // = in anon field
   * 9) D    data(0,2)      7   2  3  D=D   // found anon field
   *10) O    none           6   0  0  {     // repeat anon
   *    C    closeanonlist  0   0  1  }     // close anon list
   */
  /**
   * Parses the data file.
   * Applies the transforms defined by the class on the data U, and returns an S if all went okay.
   * Can only be used once ; further applications will produce a None.
   * This last behavior could be changed by implementing resets on all intermediary classes, but
   * this seems hardly necessary.
   * The method is protected because the layman shouldn't be able to meddle with the initial state.
   * A specific implementation will provide an actual apply method.
   * @param src source to parse
   * @param initial the initial state for the analyzer ; this lets analyze segments of data, such
   *                as a single structure ; 0 is the standard starting point.
   */
  protected def apply(src:Source,initial:Int):R = {
    val debugJump = false
    var jumped = 0
    val tkr   = new Tokenizer(src)
    val t0 = System.nanoTime
    tokenizer = tkr
    var first = tkr.start           //first token of the stack in the current state
    var tk    = first               //token under analysis ; usually first, but maybe forward exploring
    val stk   = new Array[Int](30)  //state stack ; helps manage efficiently the return state after a struct/array
    var k     = 0                   //current state (when read), next state (when set)
    //the actual thing : parse all tokens with a stack, states and transitions ; much faster/more compact than recursions.
    //aborting means running the analyzer ignoring all user calls but those closing currently open structures/arrays/lists.
    try {
      stk(0) = initial //initial state
      var v = tk.kind  //kind of the current token under scrutiny
      do {
        import Tokenizer._
        //println(tk.toString+"  => "+stk(lvl)+"/"+lvl)
        //using imbricated dense switch to benefit from the tableswitch JVM instruction
        //p is the number of poped tokens
        val p:Int = try {
          (k: @switch) match {
            case 0 => (v: @switch) match { //structure content analysis (list of sub-fields)
              case Close => if (depth==0) throw new PartialAnalyzisException(end) else try { closeStruct(tk.pos); depth-=1; k=stk(depth); } catch handler; 1
              case Equal => err(tk)
              case Data  => k=1; 0
              case _     => err(tk)
            }
            case 1 => (v) match { //field equal: fld = value
              case Equal => k=2; 0
              case _     => err(tk)
            }
            case 2 => (v: @switch) match { //field analysis
              case Open  => if (try { ignore(first) } catch handler) { k=7; 3 } else { k=3; 0 }             //substructure
              case Close => err(tk)
              case Equal => err(tk)
              case Data  => try { if (!ignore(first)) putField(first,first.next2) } catch handler; k=0; 3   //basic field
              case _     => err(tk)
            }
            case 3 => (v: @switch) match { //substructure analysis
              case Open  => try { putArray(first,tk.pos); depth+=1; stk(depth)=6 } catch handler; k=6; 3   //is array
              case Close => try { putEmpty(first,tk.prev.pos,tk.pos) } catch handler; k=0; 4               //is empty
              case Equal => err(tk)
              case Data  => k=4; 0        //don't know yet : list or struct ?
              case _     => err(tk)
            }
            case 4 => (v: @switch) match { //list/struct disambiguation
              case Close => try { putList(first,first.next2.pos); depth+=1; stk(depth)=5 } catch handler; k=5; 3    //single elt list
              case Equal => try { putStruct(first,first.next2.pos); depth+=1; stk(depth)=0 } catch handler; k=0; 3  //substructure
              case Data  => try { putList(first,first.next2.pos); depth+=1; stk(depth)=5 } catch handler; k=5; 3    //multi elt list
              case _     => err(tk)
            }
            case 5 => (v: @switch) match { //list iteration
              case Close => try { closeList(tk.pos); depth-=1; k=stk(depth) } catch handler; 1
              case Equal => err(tk)
              case Data  => try { appendList(tk) } catch handler; 1                 //iterate on same state
              case _     => err(tk)
            }
            case 6 => (v: @switch) match { //array iteration
              case Open  => try { putAnonStruct(tk.pos); depth+=1; stk(depth)=0 } catch handler; k=0; 1   //elt content
              case Close => try { closeArray(tk.pos);  depth-=1; k=stk(depth) } catch handler; 1        //end of array
              case _     => err(tk)
            }
            case 7 => throw new AnalyzerAbortException(0,-1)
          }
        } catch {
          case AnalyzerAbortException(drop,discard) =>
            var pos0 = tkr.position
            var n=drop      //number of frames to abort
            var d=discard   //number of frames to discard ; -1 indicates an ignore and not an actual frame drop
            if (n>depth)
              throw new AnalyzerException(null,s"too many frames to drop: $n requested, current depth=$depth")
            do {
              if (depth==0 && d>=0) { //top frame and not skipping a non opened struct/list : no need to read! we're straight going to the end!
                first.next.kind=Tokenizer.End
                first.next.pos=tkr.data.length
                n-=1
              } else {
                var l=1       //going to the next drop
                do { if      (first.kind==Open)  l+=1
                     else if (first.kind==Close) l-=1
                     if (l!=0) { first.toFill.fill; first=first.next }
                } while(l!=0)
                stk(depth) match {
                  case 0 => if (d>0) abortStruct(first.pos) else if (d==0) closeStruct(first.pos)
                  case 5 => if (d>0) abortList(first.pos)   else if (d==0) closeList(first.pos)
                  case 6 => if (d>0) abortArray(first.pos)  else if (d==0) closeArray(first.pos)
                }
                if (d>0)  d-=1      //count frame aborts
                if (d>=0) depth-=1  //d<0 indicates ignore ; depth doesn't change
                n-=1
                if (n>0) { first.toFill.fill; first=first.next }
              }
            } while (n>0)
            if (debugJump) {
              jumped += tkr.position-pos0
              println(s"END JUMP ${tkr.position-pos0} / $jumped")
            }
            k=stk(depth); 1  //after closing brace
          case e:PartialAnalyzisException => throw e
        }
        //Normal Code
        //tk = if (p==0) tk.next else { first=first.pop(p); first }
        //Optimized code : removes exceedingly costly call to pop (why that much ???)
        tk = if      (p==0)   tk.next
             else if (p==1) { first.toFill.fill; first=first.next; first }
             else           { var x=p; var t=first.toFill; do { x-=1; t.fill; t=t.next } while (x>0); first=t.toRead; first }
        v = tk.kind
      } while(v>=0)
      if (depth!=0 || k!=0) err(tk)
      try {
        println(System.nanoTime-t0)
        end
      } catch { //this is not an analyzer problem
        case e:Throwable => throw new NonAnalyzerException(e)
      }
    } catch {
      case NonAnalyzerException(e)    => throw e
      case e:PartialAnalyzisException => throw e
      case AnalyzerException(e,msg)   => println(first.stackString(tk,false))
                                         println(first.stackString(tk,true))
                                         if (e!=null) e.printStackTrace
                                         println(msg)
                                         throw e
      case e:Throwable                => println(first.stackString(tk,false))
                                         println("current state = "+k)
                                         println(first.stackString(tk,true))
                                         throw e
    }
  }  

}