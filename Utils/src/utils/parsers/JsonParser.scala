package utils.parsers
import scala.annotation.switch
import scala.annotation.tailrec
import scala.MatchError
import utils.CharReader

/**
 * This utility reads JSON syntax.
 * 
 * @param maxSz, largest single string expected
 * @param maxDepth, largest depth expected (counting objects and arrays)
 * @param nlInString, true if an unquoted newline (\n) is accepted in a string
 * @param withComments, true if #...\n comment style is allowed
 * 
 * WARNING:
 *   This code has been scrutinized for extended optimization.
 *   It is recommended this class not be changed!
 *   Most notably, it is difficult (impossible ?) to move some utility functions (readQuote for example)
 *   out (either in object or super class) without breaking inlining (which provides large boosts.)
 */
abstract class JsonParser(maxSz:Int,maxDepth:Int,nlInString:Boolean,withComments:Boolean) extends Handler {
  import ParserUtilities._
  
  val internalBufferError      = new Internal(s"internal string buffer too small ($maxSz)")
  val unclosedStringEODError   = new Internal("unclosed string at end of data")
  val unclosedStringError      = new Internal("unclosed string")
  val internalFrameBufferError = new Internal(s"internal frame buffer too small ($maxDepth)")
  
  //analysis environment
  final private class Info extends State {
    //possible character states
    final val Str     = 0
    final val Open    = 1
    final val Close   = 2
    final val Eq      = 3
    final val End     = 4
    final val Exit    = 5
    final val Quote   = 6
    final val OpenA   = 7
    final val CloseA  = 8
    final val NewLine = 9
    final val Space   = 10
    final val Comment = 11  

    @inline var depth:Int=0    //current depth
    @inline var str:String=_   //current string data; updated by either readQuote or readData
    @inline var line:Int=_     //current line
    //return from struct => either in anonymous list (6), or in named element (0)
    //return from list   => always in named element (0)
    @inline val frame = new Array[Byte](maxDepth)   //stack of frames
    @inline val idx   = new Array[Int](maxDepth)    //stack of indexes for arrays
    @inline val buf   = new Array[Char](maxSz)      //buffer for building string
    //character classes. Putting that array here loads it on the stack (much better perfs)
    @inline val marks0 = new Array[Byte](128);      //character classes
    for (i <- 0 to ' ') marks0(i) = Space
    marks0('{')=Open
    marks0('}')=Close
    marks0('[')=OpenA
    marks0(']')=CloseA
    marks0(':')=Eq
    marks0(',')=End
    marks0('"')=Quote
    marks0('#')= if (withComments) Comment else Str
    marks0('\n')=NewLine
    
    @inline private[this] def err(kind:Int,state:Int,msg:String) = JsonParser.this.err(this,info(kind,state),msg)
    @inline protected[this] final def marks(i:Int) = if ((i&0xFFFFFF80)!=0) Str else marks0(i&0x7F)
   
    protected[this] final def innerHandler:PartialFunction[Throwable,Unit] = {
      case e:Jump      => throw e
      case e:Throwable => handler(e)
    }
    
    //reads a quoted string with escape sequences
    //cannot return a String, because the inliner doesn't like it...
    //cannot put elsewhere (object/superclass) as utility, because the inliner doesn't like it...
    //method is extremely delicate when comes inlining; and inlining really improves perfs.
    @inline @tailrec private[this] final def readQuote(b:Int,d:CharReader):Unit = {
      val c = (try { d.nextChar } catch { case CharReader.eod => throw unclosedStringEODError }) match {
        case '\\' => ((try { d.nextChar } catch { case CharReader.eod => throw unclosedStringEODError }): @switch) match {
                       case 'b'  => '\b'
                       case 'f'  => '\f'
                       case 'n'  => '\n'
                       case 'r'  => '\r'
                       case 't'  => '\t'
                       case 'u'  => val u=new Array[Char](4)
                                    try {
                                      u(0) = d.nextChar
                                      u(1) = d.nextChar
                                      u(2) = d.nextChar
                                      u(3) = d.nextChar
                                      Integer.parseInt(new String(u),16).asInstanceOf[Char]
                                    } catch {
                                      case _:NumberFormatException => throw new Internal("illegal unicode character: $u")
                                      case CharReader.eod          => throw unclosedStringEODError
                                    }
                       case e    => e
                     }
          case '"'  => str=new String(buf,0,b); return
          case '\n' => if (nlInString) { line+=1; '\n' } else throw unclosedStringError
          case x    => x
        }
      try { buf(b) = c } catch { case _:ArrayIndexOutOfBoundsException => internalBufferError }
      readQuote(b+1,d)
    }
    
    //cannot return a String, because the inliner doesn't like it...
    //cannot put elsewhere (object/superclass) as utility, because the inliner doesn't like it...
    //method is extremely delicate when comes inlining; and inlining really improves perfs.
    @inline @tailrec private[this] def readData(b:Int,d:CharReader,c:Char):Unit = {
       try { buf(b)=c } catch { case _:ArrayIndexOutOfBoundsException=> throw internalBufferError }
       val c1 = try { d.nextChar } catch { case CharReader.eod => str=new String(buf,0,b+1); return }
       if (marks(c1)!=Str) { d.reject; str=new String(buf,0,b+1); return } //the last char read doesn't belong to that current token
       readData(b+1,d,c1)
    }
    
    //checks that a, unquoted string is valid
    @inline private[this] def checkData(str:String):String = {
      str.charAt(0) match { //check that the unquoted token is acceptable
         case 't' => if (str!="true")  throw new Internal(s"illegal data: $str")
         case 'f' => if (str!="false") throw new Internal(s"illegal data: $str")
         case 'n' => if (str!="null")  throw new Internal(s"illegal data: $str"); null
         case _   => try { java.lang.Double.parseDouble(str) } catch { case _:NumberFormatException => throw new Internal(s"illegal number format: $str") }
      }
      str
    }
   
    //returns either the kind of token (Byte) bundled with the length of a string (3 upper bytes)
    @inline @tailrec final private[this] def read(d:CharReader,spy:Spy):Int = {
      try {
        var c=d.nextChar
        (marks(c): @switch) match {
          case Str=>     readData(0,d,c); checkData(str); return Str
          case Open=>    return Open
          case Close=>   return Close
          case OpenA=>   return OpenA
          case CloseA=>  return CloseA
          case Eq=>      return Eq
          case End=>     return End
          case Quote=>   readQuote(0,d); return Quote
          case Space=>   
          case NewLine=> line+=1; spy.line = line
          case Comment=> do { c=d.nextChar } while (c!='\n'); line+=1; spy.line = line
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
        case Open|OpenA    => n+=1
        case Close|CloseA  => n-=1; if (n<0) { d.reject; return }
        case Quote         => var old=c
                              do {
                                old=c
                                c = try { d.nextChar } catch { case CharReader.eod => throw unclosedStringEODError }
                                if (c=='\n') if (nlInString) line+=1 else throw unclosedStringError else line+=1
                              } while (marks(c)!=Quote || old=='\\')
        case Comment       => do { c=d.nextChar } while (c!='\n'); line+=1
        case NewLine       => line+=1
        case _             =>
      }
      readFast(d,n)
    }

    //returns the next state after this switch.
    //Two cases: switch branch is goto: state changes, no token read. switch branch is return: state may change, new token is read.
    @inline @tailrec final private[this] def doSwitch(kind:Int,go:Int,spy:Spy):Int = {
      //using imbricated dense switch to benefit from the tableswitch JVM instruction (verified.)
      //trying one big switch is self defeating (worse perfs due to holes and either a lookupswitch or an intermediary state table)
      (go: @switch) match {
        case 0=> (kind: @switch) match { //object start
          case Open   => try { depth+=1; frame(depth)=9 } catch { case _:ArrayIndexOutOfBoundsException=> throw internalFrameBufferError }; try { spy.depth=depth } catch innerHandler; return 4   //object
        }
        case 1=> (kind: @switch) match { //array start
          case OpenA  => try { depth+=1; frame(depth)=11; idx(depth)=0 } catch { case _:ArrayIndexOutOfBoundsException=> throw internalFrameBufferError }; try { spy.depth=depth } catch innerHandler; return 7   //array
        }
        case 2=> (kind: @switch) match { //closing object
          case Close  => if (depth<=0) err(kind,2,s"closing a frame at depth $depth is illegal"); depth-=1; var r=frame(depth); try { spy.depth=depth; pull(this) } catch innerHandler; return r
        }
        case 3=> (kind: @switch) match { //closing object or array
          case CloseA => if (depth<=0) err(kind,2,s"closing a frame at depth $depth is illegal"); depth-=1; var r=frame(depth); try { spy.depth=depth; pull(this) } catch innerHandler; return r
        }
        case 4=> (kind: @switch) match {      //after object start
          case Close => doSwitch(kind,2,spy)  //empty
          case Quote => doSwitch(kind,5,spy)  //name read
        }
        case 5=> (kind: @switch) match {      //after object start
          case Quote => try { push(this,str) } catch innerHandler; return 6  //name read
        }
        case 6=> (kind: @switch) match {      // = now pass on equal
          case Eq    => return 8              //value expected
        }
        case 7=> (kind: @switch) match { //after array start
          case CloseA => doSwitch(kind,3,spy)   //empty
          case _      => doSwitch(kind,10,spy)  //value expected
        }
        case 8=> (kind: @switch) match { //read value in object
          case Open      => doSwitch(kind,0,spy) //sub object
          case OpenA     => doSwitch(kind,1,spy) //sub array
          case Str|Quote => try { pull(this,str) } catch innerHandler; 9 //value
        }
        case 9=> (kind: @switch) match {
          case Exit  => if (depth>0) err(kind,0,"premature end of data") else throw exit
          case Close => doSwitch(kind,2,spy)
          case End   => return 5
        }
        case 10=> try { val i=idx(depth); idx(depth)+=1; push(this,i) } catch innerHandler; (kind: @switch) match { //read value in array
           case Open      => doSwitch(kind,0,spy) //sub object
           case OpenA     => doSwitch(kind,1,spy) //sub array
           case Str|Quote => try { pull(this,str) } catch innerHandler; return 11
        }
        case 11=> (kind: @switch) match {
          case CloseA => doSwitch(kind,3,spy)
          case End    => return 10
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
          case _:MatchError                    => err(k,go,s"illegal transition")
        }
      } catch {
        case e:Internal => err(Str,go,e.msg)
      }
      doFrame(d,spy,r)
    }
  
    final def apply(d:CharReader,spy:Spy) = try { frame(0)=9; doFrame(d,spy,8) } catch { case `exit`=> }
    
    def info(kind:Int,state:Int):String = s"transition from  state $state with kind $kind at line $line and depth $depth ; last token known: '$str'"
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
  final def apply(d:CharReader):Unit = (new Info)(d,noSpy)
  final def abort(n:Int) = throw new ParserUtilities.Jump(n)
}
