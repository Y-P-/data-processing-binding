package utils.parsers
import scala.annotation.switch
import scala.annotation.tailrec
import scala.MatchError
import utils.CharReader

/**
 * This utility reads a standard hierarchical syntax.
 * e.g. "a={u=2 v={x=4 z={j=0}};w=6};b=\"\"\"5\"\"\";t={z=9;r=7};vv={2;3;4};w={{a=3;b={x;y}}{a=3;b={x;y}}};f=\"z\"\"\"\"\""
 * 
 * The list of escaped characters cannot include '-' nor '+' as translated values. These are restricted, the first for forbidden
 * sequences (the default), the second for signaling a coded unicode character, on 4 hexdigits. So if you put ('u','+') in the escape
 * list, then \u0065 will translate to the single character 'e' (which has the unicode value of 0x0065.) The escape character always
 * escapes to itself (i.e if # is the escape character, then ## translates to '#')
 * 
 * @param open, the opening character for an object or array (usually {[(<)
 * @param close, the closing character for an object or array (usually }])>)
 * @param equal, the character between name and value (usually =:~-)
 * @param sep, the character separating sequenced elements. Optional (put 0 if you don't use it) ; otherwise usually ,; It is treated as a space.
 * @param quote, the character introducing a quoted string. (usually '"`)
 * @param comment, the character introducing a comment that last until the end of the line. Optional (put 0 if you don't use it)
 * @param esc, the escape character. Optional, use 0 if you don't use it
 * @param maxSz, largest single string expected
 * @param maxDepth, largest depth expected (counting objects and arrays)
 * @param nlInString, true if an unquoted newline (\n) is accepted in a string
 * @param withComments, true if #...\n comment style is allowed
 * 
 * WARNING:
 *   This code has been scrutinized for extended optimization.
 *   It is recommended this class not be changed!
 */
abstract class StructParser(open:Char,close:Char,equal:Char,sep:Char,quote:Char,comment:Char,maxSz:Int,maxDepth:Int,nlInString:Boolean,esc:Char,escapes:Array[(Char,Char)]) extends Handler {
  import ParserUtilities._
  
  val internalBufferError      = new Internal(s"internal string buffer too small ($maxSz)")
  val unclosedStringEODError   = new Internal("unclosed string at end of data")
  val unclosedStringError      = new Internal("unclosed string")
  val internalFrameBufferError = new Internal(s"internal frame buffer too small ($maxDepth)")
  
  @inline val escape0 = new Array[Char](128);      //list for escaped chars
  if (esc!=0) {
    for (i <- 0 to 127) escape0(i) = '-'           //forbidden sequence, the default
    for (x <- escapes)  escape0(x._1)=x._2
    escape0(esc) = esc
    escape0(quote) = quote
  }
  
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
    final val NewLine = 7
    final val Space   = 8
    final val Comment = 9
    
    final def line  = line0
    final def depth = depth0
    final def str   = str0

    @inline protected[this] var depth0:Int=0    //current depth
    @inline protected[this] var str0:String=_   //current string data; updated by either readQuote or readData
    @inline protected[this] var str1:String=_   //current string data; updated by either readQuote or readData
    @inline protected[this] var line0:Int=1     //current line
    //return from struct => either in anonymous list (6), or in named element (0)
    //return from list   => always in named element (0)
    @inline val frame   = new Array[Byte](maxDepth)   //stack of frames
    @inline val idx     = new Array[Int](maxDepth)    //stack of indexes for arrays
    @inline val buf     = new Array[Char](maxSz)      //buffer for building string
    @inline val unicode = new Array[Char](4)          //buffer for unicode
    //character classes. Putting that array here loads it on the stack (much better perfs)
    @inline val marks0 = new Array[Byte](128);      //character classes
    @inline val quoteClass0 = new Array[Byte](128);  //class for quoted characters
    for (i <- 0 to ' ') marks0(i) = Space
    marks0(open)=Open
    marks0(close)=Close
    marks0(equal)=Eq
    if (sep!=0) marks0(sep)=End
    marks0(quote)=Quote
    if (comment!=0) marks0(comment)=Comment
    marks0('\n')=NewLine
    if (esc!=0) quoteClass0(esc)=1 //escaped char
    quoteClass0(quote)=2 //end of string
    quoteClass0('\n')=3  //new line
    
    @inline private[this] final def err(kind:Int,state:Int,msg:String) = StructParser.this.err(info(kind,state),msg)
    //execution shows this removes array limits checks (in addition to being necessary!)
    @inline private[this] final def marks(i:Int)      = if ((0xFFFFFF80&i)==0) marks0(i&0x7F) else Str
    @inline private[this] final def escape(i:Int)     = if ((0xFFFFFF80&i)==0) escape0(i)     else '-'
    @inline private[this] final def quoteClass(i:Int) = if ((0xFFFFFF80&i)==0) quoteClass0(i) else 0
   
    protected[this] final def innerHandler:PartialFunction[Throwable,Unit] = {
      case e:Jump      => throw e
      case e:Throwable => handler(e)
    }
    
    //reads a quoted string with escape sequences
    //cannot return a String, because the inliner doesn't like it...
    //cannot put elsewhere (object/superclass) as utility, because the inliner doesn't like it...
    //method is extremely delicate when comes inlining; and inlining really improves perfs.
    @inline @tailrec private[this] final def readQuote(b:Int,d:CharReader):Unit = {
      val c0 = (try { d.nextChar } catch { case CharReader.eod => throw unclosedStringEODError })
      val c1 = quoteClass(c0) match {
        case 1 => val c1 = escape(try { d.nextChar } catch { case CharReader.eod => throw unclosedStringEODError })
                  if (c1=='+') {
                    try {
                      unicode(0) = d.nextChar
                      unicode(1) = d.nextChar
                      unicode(2) = d.nextChar
                      unicode(3) = d.nextChar
                      Integer.parseInt(new String(unicode),16).asInstanceOf[Char]
                    } catch {
                      case _:NumberFormatException => throw new Internal("illegal unicode character: $u")
                      case CharReader.eod          => throw unclosedStringEODError
                    }
                  }
                  else if (c1=='-') throw new Internal("illegal escape sequence")
                  else c1
          case 2  => str0=new String(buf,0,b); return
          case 3  => if (nlInString) { line0+=1; '\n' } else throw unclosedStringError
          case _  => c0
        }
      try { buf(b) = c1 } catch { case _:ArrayIndexOutOfBoundsException => internalBufferError }
      readQuote(b+1,d)
    }
    
    //cannot return a String, because the inliner doesn't like it...
    //cannot put elsewhere (object/superclass) as utility, because the inliner doesn't like it...
    //method is extremely delicate when comes inlining; and inlining really improves perfs.
    @inline @tailrec private[this] def readData(b:Int,d:CharReader,c:Char):Unit = {
       try { buf(b)=c } catch { case _:ArrayIndexOutOfBoundsException=> throw internalBufferError }
       val c1 = try { d.nextChar } catch { case CharReader.eod => str0=new String(buf,0,b+1); return }
       if (marks(c1)!=Str) { d.reject; str0=new String(buf,0,b+1); return } //the last char read doesn't belong to that current token
       readData(b+1,d,c1)
    }
    
    //returns either the kind of token (Byte) bundled with the length of a string (3 upper bytes)
    @inline @tailrec final private[this] def read(d:CharReader):Int = {
      try {
        var c=d.nextChar
        (marks(c): @switch) match {
          case Str=>     str1=str0; readData(0,d,c); return Str
          case Open=>    return Open
          case Close=>   return Close
          case Eq=>      return Eq
          case End=>     return End
          case Quote=>   str1=str0; readQuote(0,d); return Str
          case Space=>   
          case NewLine=> line0+=1
          case Comment=> do { c=d.nextChar } while (c!='\n'); line0+=1
        }
      } catch { case CharReader.eod => return Exit }
      read(d)
    }
    //jumps over open/close until (nb open - nb close) = depth+1
    //in effect: depth=0: jump to the end of the current layer, depth=1: jump to the end of the previous layer etc.
    @inline @tailrec final private[this] def readFast(d:CharReader,n0:Int):Unit = {
      var n=n0
      var c=d.nextChar
      (marks(c): @switch) match {
        case Open    => n+=1
        case Close   => n-=1; if (n<0) { d.reject; return }
        case Quote   => var old=c
                        do {
                          old=c
                          c = try { d.nextChar } catch { case CharReader.eod => throw unclosedStringEODError }
                          if (c=='\n') if (nlInString) line0+=1 else throw unclosedStringError else line0+=1
                        } while (marks(c)!=Quote || old=='\\')
        case Comment => do { c=d.nextChar } while (c!='\n'); line0+=1
        case NewLine => line0+=1
        case _       =>
      }
      readFast(d,n)
    }

    //returns the next state after this switch.
    @inline @tailrec final private[this] def doSwitch(kind:Int,go:Int):Int = {
      //using imbricated dense switch to benefit from the tableswitch JVM instruction (verified.)
      //trying one big switch is self defeating (worse perfs due to holes and either a lookupswitch or an intermediary state table)
      (go: @switch) match {
        case 0=> (kind: @switch) match { //structure content analysis (list of sub-fields)
          case Close => if (depth0<=0) err(kind,2,s"closing a frame at depth $depth0 is illegal"); depth0-=1; var r=frame(depth); try { pull() } catch innerHandler; return r
          case Str   => try { push(str0) } catch innerHandler; return 1
          case Exit  => throw exit
         }
        case 1=> (kind: @switch) match {       //field equal: fld = value
          case Eq => return 2
        }
        case 2=> (kind: @switch) match {       //field analysis
          case Open => try { depth0+=1; frame(depth0)=0 } catch { case _:ArrayIndexOutOfBoundsException=> throw internalBufferError }; return 3   //object or array. By default, we say object.
          case Str  => try { pull(str0) } catch innerHandler; return 0   //basic field
        }
        case 3=> (kind: @switch) match {                                 //substructure analysis: first token is checked
          //is array of object/array; note the specific Jump handler: if push wants to jump, it is from the first Open, but we already are in the second.
          case Open  => frame(depth0)=6; try { push(0) } catch { case j:Jump => depth0+=1; throw new Jump(j.n+1); case e:Throwable => handler(e) }; idx(depth0)=0; doSwitch(kind,2)
          case Close => doSwitch(kind,0)                                     //is empty (by default empty array)
          case Str   => return 4                                             //don't know yet : array (of strings) or object ?
        }
        case 4=> (kind: @switch) match {       //array/object disambiguation
          //single string array; note the specific Jump handler: Open/Close have been read! so any attempt to jump must ignore the current layer
          case Close => try { frame(depth0)=5; push(0); pull(str0) } catch { case j:Jump => if (j.n>0) throw new Jump(j.n-1); case e:Throwable => handler(e) }; doSwitch(kind,0)
          //we have an object
          case Eq    => try { push(str0) } catch innerHandler; return 2 
          //multi elt array
          case Str   => try { frame(depth0)=5; push(0); pull(str1); push(1); pull(str0); idx(depth0)=1 } catch innerHandler; return 5
        }
        case 5=> (kind: @switch) match {       //string array iteration (note that we wrote frame(depth)=5, but this won't ever be used for lack of children)
          case Close => doSwitch(kind,0)
          case Str   => try { idx(depth0)+=1; push(idx(depth0)); pull(str0) } catch innerHandler; return 5   //iterate on same state
         }
        case 6=> (kind: @switch) match {       //other array iteration
          case Open  => idx(depth0)+=1; try { push(idx(depth0)) } catch innerHandler; doSwitch(kind,2)   //new substructure
          case Close => doSwitch(kind,0)   //end of array
        }
      }
    }
  
    @tailrec final private[this] def doFrame(d:CharReader,go:Int):Unit = {
      val r = try {
        val k=read(d)
        try {
          doSwitch(k,go)
        } catch {
          case j:Jump if depth==j.n || depth<0 => return
          case j:Jump if depth>j.n             => readFast(d,j.n); depth0-=j.n; 0
          case j:Jump if depth<j.n             => err(k,go,s"illegal jump out of ${j.n} frames ($depth available)")
          case _:MatchError                    => err(k,go,s"illegal transition from state $go with token $k")
        }
      } catch {
        case e:Internal => err(Str,go,e.msg)
      }
      doFrame(d,r)
    }
  
    /** Analyses the data in d.
     *  This method is not thread safe.
     */
    private[StructParser] final def apply(d:CharReader) = try {
      //reset variables ; ensures that we can reuse the same instance for consecutive runs
      depth0=0
      line0=0
      str0=null
      str1=null
      frame(0)=0
      doFrame(d,0)
    } catch {
      case `exit`=>
    }
    
    def info(kind:Int,state:Int):String = s"transition from  state $state with kind $kind at line $line and depth $depth ; last token known: '$str'"
  }
  
  final protected class Processor(i:Info) extends utils.parsers.CharProcessor {
    def apply(d:CharReader):Unit = i(d)
    def state:State = i
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
  def newProc:Processor = new Processor(new Info)
  def abort(n:Int):Nothing = throw new Jump(n)
}


