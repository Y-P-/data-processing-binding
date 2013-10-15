package utils.hparser
import scala.annotation.switch
import scala.annotation.tailrec
import scala.MatchError
import utils.CharReader

/**
 * This utility reads a standard hierarchical syntax.
 * e.g. "a={u=2 v={x=4 z={j=0}};w=6};b=\"\"\"5\"\"\";t={z=9;r=7};vv={2;3;4};w={{a=3;b={x;y}}{a=3;b={x;y}}};f=\"z\"\"\"\"\""
 * 
 * WARNING:
 *   This code has been scrutinized for extended optimization.
 *   It is recommended this class not be changed!
 */
abstract class HParser(open:Char,close:Char,equal:Char,sep:Char,quote:Char,comment:Char,maxSz:Int,maxDepth:Int) extends Handler {
  import HParser._
  
  //analysis environment
  final protected class Info extends State {
    //return from struct => either in anonymous list (6), or in named element (0)
    //return from list   => always in named element (0)
    @inline val frame = new Array[Byte](maxDepth)   //stack of frames
    @inline val buf   = new Array[Char](maxSz)      //buffer for building string
	//character classes. Putting that array here loads it on the stack (much better perfs)
	@inline val marks = new Array[Byte](256);       //character classes
	for (i <- 0 to ' ') marks(i) = Space
    marks(open)=Open
    marks(close)=Close
    marks(equal)=Eq
    marks(sep)=End
    marks(quote)=Quote
    marks(comment)=Comment
    marks('\n')=NewLine
    //@inline var go:Byte=_
    @inline var depth:Int=_    //current depth
    @inline var line:Int=_     //current line
    @inline var str0:String=_  //last string data (only when appropriate)
    @inline var str:String=_   //current string data
    
    @inline private[this] def err(kind:Int,state:Int,msg:String) = HParser.this.err(this,info(kind,state),msg)
    
    private[this] def innerHandler:PartialFunction[Throwable,Unit] = {
	  case e:Jump      => throw e
	  case e:Throwable => handler(e)
	}
  
    //returns either the kind of token (Byte) bundled with the length of a string (3 upper bytes)
    @inline @tailrec final private[this] def read(d:CharReader,spy:Spy):Int = {
      try {
        var c=d.nextChar
        (marks(c): @switch) match {
          case Str=>     var b=0  //fill up the buffer
                         str0=str
                         try { do { try { buf(b)=c; b+=1 } catch { case _:ArrayIndexOutOfBoundsException=> throw new Internal(s"internal string buffer too small (${buf.length})") }; c=d.nextChar; } while (marks(c)==Str); } catch { case CharReader.eod => } //eod is the next token: ignore
                         d.reject //the last char read doesn't belong to that current token
                         str=new String(buf,0,b)
                         return Str
          case Open=>    return Open
          case Close=>   return Close
          case Eq=>      return Eq
          case End=>     
          case Quote=>   var b=0
                         str0=str
                         do {
                           do {
                             c = try { d.nextChar } catch { case CharReader.eod => throw new Internal("unclosed string at end of data") }
                             if (c=='\n') throw new Internal("unclosed string")
                             try { buf(b)=c; b+=1 } catch { case _:ArrayIndexOutOfBoundsException=> throw new Internal(s"internal string buffer too small (${buf.length})") }
                           } while (marks(c)!=Quote)
                           try { c=d.nextChar } catch { case CharReader.eod => c=0 } //eod here can be ignored: it will be the next token
                         } while (marks(c)==Quote)
                         d.reject                   //the last char read (which comes after the last quote) doesn't belong to that current token
                         str=new String(buf,0,b-1)  //don't use the last quote read in the resulting string
                         return Str
          case Comment=> do { } while (d.nextChar != '\n')
          case Space=>   
          case NewLine=> line+=1; spy.line = line
        }
      } catch { case CharReader.eod => return Exit }
      read(d,spy)
    }
    //jumps over open/close until (nb open - nb close) = depth+1
    //in effect: depth=0: jump to the end of the current layer, depth=1: jump to the end of the previous layer etc.
    @inline @tailrec final private[this] def readFast(d:CharReader,n0:Int):Unit = {
      var n=n0
      var c=d.nextChar
      (marks(c): @switch) match {
        case Open=>    n+=1
        case Close=>   n-=1; if (n<0) { d.reject; return }
        case Quote=>   do {  //almost a copy from the Quote in read. We have to analyze Quoted strings in order to correctly account for Open/Close
                         do {
                           c = try { d.nextChar } catch { case CharReader.eod => throw new Internal("unclosed string at end of data") }
                           if (c=='\n') throw new Internal("unclosed string")
                         } while (marks(c)!=Quote)
                         try { c=d.nextChar } catch { case CharReader.eod => c=0 }
                       } while (marks(c)==Quote)
                       d.reject
        case Comment=> do { } while (d.nextChar != '\n')
        case NewLine=> line+=1
        case _=>
      }
      readFast(d,n)
    }

    //returns the next state after this switch. go contains the next state in the low byte, the stack depth in the upper byte
    @inline @tailrec final private[this] def doSwitch(kind:Int,go:Int,spy:Spy):Int = {
      //using imbricated dense switch to benefit from the tableswitch JVM instruction (verified.)
      //trying one big switch is self defeating (worse perfs due to holes and either a lookupswitch or an intermediary state table)
      (go: @switch) match {
        case 0=> (kind: @switch) match { //structure content analysis (list of sub-fields)
          case Close => depth-=1; if (depth<0) err(kind,0,"closing a frame at depth 0 is illegal") else try { spy.depth=depth; pull(this) } catch innerHandler; frame(depth)
          case Str   => try { push(this,str) } catch innerHandler; 1
          case Exit  => throw CharReader.eod
         }
        case 1=> (kind: @switch) match { //field equal: fld = value
          case Eq => 2
        }
        case 2=> (kind: @switch) match { //field analysis
          case Open => try { frame(depth)=0 } catch { case _:ArrayIndexOutOfBoundsException=> throw new Internal(s"internal frame buffer too small (${frame.length})") }; depth+=1; spy.depth=depth; 3   //substructure or sublist
          case Str  => try { pull(this,str) } catch innerHandler; 0   //basic field
        }
        case 3=> (kind: @switch) match {   //substructure analysis: first token
          case Open  => doSwitch(kind,6,spy)   //is array of anonymous stc
          case Close => doSwitch(kind,0,spy)   //is empty
          case Str   => 4                  //don't know yet : list (of strings) or struct (named) ?
        }
        case 4=> (kind: @switch) match { //list/struct disambiguation
          case Close => try { push(this,""); pull(this,str0) } catch innerHandler; doSwitch(kind,0,spy) //single elt list
          case Eq    => try { push(this,str0) } catch innerHandler; 2  //we have a substructure
          case Str   => try { push(this,""); pull(this,str0); push(this,""); pull(this,str) } catch innerHandler; 5 //multi elt list
        }
        case 5=> (kind: @switch) match { //list iteration
          case Close => doSwitch(kind,0,spy)
          case Str   => try { push(this,""); pull(this,str) } catch innerHandler; 5   //iterate on same state
         }
        case 6=> (kind: @switch) match { //array iteration
          case Open  => try { push(this,"") } catch innerHandler; try { frame(depth)=6 } catch { case _:ArrayIndexOutOfBoundsException=> throw new Internal(s"internal frame buffer too small (${frame.length})") }; depth+=1; spy.depth=depth; 0 //new substructure
          case Close => 0                                                           //end of array
        }
      }
    }
  
    @tailrec final private[this] def doFrame(d:CharReader,spy:Spy,go:Int):Unit = {
      val r = try {
        val k=read(d,spy)
        try {
          doSwitch(k,go,spy)
        } catch {
          case j:Jump if depth==j.n || depth<0 => return
          case j:Jump if depth<j.n             => readFast(d,j.n+1); depth-=j.n; 0
          case j:Jump if depth>j.n             => err(k,go,s"illegal jump out of ${j.n} frames ($depth available)")
          case _:MatchError                    => err(k,go,s"illegal transition from state $go with token $k")
        }
      } catch {
        case e:Internal => err(Str,go,e.msg)
      }
      doFrame(d,spy,r)
    }
  
    private[HParser] final def apply(d:CharReader,spy:Spy) = doFrame(d,spy,0)
    
    def info(kind:Int,state:Int):String = {
      val current = state match {
        case 0=> kind match {
          case Close => "}"
          case Str   => str
          case Exit  => "<eod>"
         }
        case 1=> kind match {
          case Eq => s"$str ="
        }
        case 2=> kind match {
          case Open => s"$str = {"
          case Str  => s"= $str"
        }
        case 3=> kind match {
          case Open  => s"$str = { {"
          case Close => s"$str = { }"
          case Str   => s"$str = {"
        }
        case 4=> kind match {
          case Close => s"$str0 = { $str }"
          case Eq    => s"$str0 = { $str ="
          case Str   => s"= { $str0 $str"
        }
        case 5=> kind match {
          case Close => "}"
          case Str   => s"$str0 $str"
        }
        case 6=> kind match {
          case Open  => "{"
          case Close => s"$str }"
        }
      }
      s"<$current> at line $line and depth $depth"
    }
  }

  /**
   * Runs the parser on the given CharReader.
   * Doing this invokes the push/pull (abstract) methods of this class.
   * This call will only succeed if the data respects the grammar.
   * 
   * Errors:
   * - An incorrect grammar will cause the invocation of err which MUST end up throwing an exception.
   *   The user is free to choose what exception is thrown. Ideally, it should not conflict with any exception from user code.
   * - If the user code (push/pull) throws any exception, it will be handled by the handler (abstract) method provided by the user.
   *   This may or may not halt the parser (the parser can go on as long as the data respects the grammar.)
   *   However, if the user code/handler lets MatchError pass on, then the parser will not continue ; this should be avoided as this may
   *   be confusing: as a rule, the user code/handler should handle MatchError.
   * - The user code can call abort(n) to escape to the end of the nth frame layer (above) in an accelerated way (no client code called)
   *   and reduced parsing overhead.
   * - Be warned that the parser may throw ArrayIndexOutOfBoundsException if either the string buffer is too small (a string larger than
   *   expected is encountered)
   * 
   * This is thread safe, except possibly spy which is used for update if so requested.
   */
  final def apply(d:CharReader,spy:Spy):Unit = (new Info)(d,spy)
  final def apply(d:CharReader):Unit = (new Info)(d,HParser.noSpy)
}

object HParser {
  //possible states
  final val Str     = 0
  final val Open    = 1
  final val Close   = 2
  final val Eq      = 3
  final val End     = 4
  final val Exit    = 5
  final val Comment = 6
  final val Space   = 7
  final val Quote   = 8
  final val NewLine = 9
  
  private class Jump(val n:Int) extends Exception {
    override def fillInStackTrace = this    
  }
  private class Internal(val msg:String) extends Exception {
    override def fillInStackTrace = this    
  }
  
  private object noSpy extends Spy {
    def line:Int=0
    def depth:Int=0
    def line_=(l:Int) = {}
    def depth_=(l:Int) = {}
  }
  
  def abort(n:Int):Nothing = throw new Jump(n)
}

