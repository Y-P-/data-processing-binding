package parser.px.analyzers

/**
 * Using struct analyzers usually requires a multi step settup.
 * The most complex step is step two. Most other steps require but simple tasks.
 * - Define the kind of information kept from the parsed tokens (Tokenizer.Token)
 *   This can be as simple as Strings, or complex including data positions etc.
 * - Define the actual field structures ; these would likely extend the
 *   parser.px.analyzers.fields.Field[TK] sub classes, mostly because the
 *   basic classes offer almost no service ; these classes will implement
 *   services pertinent to your application.
 *   You can also redefine these classes as long as you respect the required interface.
 * - implement a version of parser.px.analyzers.fields.FieldStructAnalyzer[TK]
 *   corresponding to the classes you laid down on step two. Most likely, this
 *   means calling constructors.
 * - implement filters if necessary (the default implementation turns filters off)
 * - implement a StandardStructAnalyzer to plug all the remaining holes, which means
 *   implementing the buildToken method (likely a constructor from step one), and an
 *   en method, which is your own...
 */

import parser.Source
import parser.px.Analyzer
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack

/**
 * This trait provides a change of paradygm for the analyzer.
 * 
 * Rather than calling methods on the flys, as tokens are read, this waits
 * for full structure completion before building anything.
 * This means that the deepest nested data is checked first, and last is the
 * whole container data.
 * 
 * Stacking this traits means that you only have to provide the required builders for your fields
 * and Token. Reminder : TKN cannot be Tk, because the lifetime of Tk is very short.
 */
trait StructAnalyzer extends Analyzer {
  protected type R = DSR     // The return type for the analyzer ; usually DSR
  protected type Tk          // The entering token class
  protected type TKN         // Class used for converting Tokens to names ; you cannot keep Tokens (Tk) that are passed as their lifetime is short 
  protected type Dl          // List field element ; doesn't belong to the Fld hierarchy
  protected type Fld         // Base class for all fields which can be referenced from a Struct
  protected type DF  <: Fld  // Base field
  protected type DL  <: Fld  // List field
  protected type DSN <: Fld  // Struct field (normal)
  protected type DSA <: Fld  // Struct field (anonymous)
  protected type DSR <: Fld  // Struct field (root)
  protected type DA  <: Fld  // Array field
  protected type DE  <: Fld  // Empty field
  protected def buildName(t:Tk):TKN
  protected def buildField(name:Tk,data:Tk):DF                               // Here, we can use directly Tk because it doesn't have to be stored
  protected def buildList(name:TKN,values:Seq[Dl],open:Int,close:Int):DL     // List
  protected def buildStruct(name:TKN,fields:Seq[Fld],open:Int,close:Int):DSN // Normal structure
  protected def buildStruct(fields:Seq[Fld],open:Int,close:Int):DSA          // Anonymous structure
  protected def buildRoot(fields:Seq[Fld],open:Int,close:Int):DSR            // Root structure
  protected def buildArray(name:TKN,fields:Seq[Fld],open:Int,close:Int):DA   // Array element
  protected def buildEmpty(name:Tk,open:Int,close:Int):DE                    // Empty structure/array/list
  protected def buildListElt(name:Tk):Dl                                     // List element
  
  /**
   * Temporary holding area to keep track of the current structure/list being built.
   */
  protected class Content[K] { self =>
    var tk:TKN  = _
    var pos:Int = _
    val stk     = new PStack[K](null.asInstanceOf[K])
  }
  /**
   * A specific array-stack.
   * It's holding area is not recreated on clear ; objects on the stack are not emptied ; they are reused.
   * Avoids recreating temporary, short lived, objects.
   * As a result, it's content must be immediately used.
   * 
   * As a sequence, it performs as FIFO (keeps field natural ordering).
   */
  protected class PStack[K](build: =>K) extends Seq[K] {
    var a = new Array[AnyRef](30)
    var idx = 0;
    init(0,a.length-1)
    def init(min:Int,max:Int) = { var l=min; do { a(l)=build.asInstanceOf[AnyRef]; l+=1 } while (l<=max) }
    def grow() = {
      if (idx==a.length) {
        val l0 = a.length
        val l1 = l0+l0/2
        var aa = new Array[AnyRef](l1)
        System.arraycopy(a,0,aa,0,a.length)
        a = aa
        init(l0,l1-1)
      }
    }
    def push(k:K):Unit = { grow; a(idx) = k.asInstanceOf[AnyRef]; idx += 1 }
    def clear() = idx=0
    def pop:K = { idx-=1; a(idx).asInstanceOf[K] }
    def top:K = a(idx-1).asInstanceOf[K]
    def length = idx
    def apply(i:Int) = a(i).asInstanceOf[K]
    def iterator = new Iterator[K] { var i = -1; val max = idx-1
      def hasNext = i<max
      def next    = a({i+=1; i}).asInstanceOf[K]
    }
  }
  /**
   * The stack of current structures being built.
   */
  protected object stackS extends PStack[Content[Fld]](new Content[Fld]) {
    push(null.asInstanceOf[TKN],0)  //root structure
    def build = new Content[Fld]
    def push(tk:TKN,pos:Int):Unit = {
      grow
      val v = a(idx).asInstanceOf[Content[Fld]]
      idx += 1
      v.tk  = tk
      v.pos = pos
    }
  }
  
  def apply(src:Source):DSR = super.apply(src,0)
  
  /** The list being built */
  protected val list = new Content[Dl]
  
  /* These plug the hole to the standard analyzer */
  protected final def putEmpty(name:Tk,open:Int,close:Int):Unit = { stackS.top.stk.push(buildEmpty(name,open,close)) }
  protected final def putList(name:Tk,open:Int):Unit            = { list.tk=buildName(name); list.pos=open }
  protected final def appendList(value:Tk):Unit                 = { list.stk.push(buildListElt(value)) }
  protected final def closeList(close:Int):Unit                 = { stackS.top.stk.push(buildList(list.tk,list.stk,list.pos,close)); list.stk.clear }
  protected final def putStruct(name:Tk,open:Int):Unit          = { stackS.push(if (name!=null) buildName(name) else null.asInstanceOf[TKN],open) }
  protected final def putField(name:Tk,data:Tk):Unit            = { stackS.top.stk.push(buildField(name,data)) }
  protected final def closeStruct(close:Int):Unit               = { val s=stackS.pop; stackS.top.stk.push(if (s.tk!=null) buildStruct(s.tk,s.stk,s.pos,close) else buildStruct(s.stk,s.pos,close)); s.stk.clear }
  protected final def putArray(name:Tk,open:Int):Unit           = { stackS.push(buildName(name),open) }
  protected final def putAnonStruct(open:Int):Unit              = { putStruct(null,open) }
  protected final def closeArray(close:Int):Unit                = { val s=stackS.pop; stackS.top.stk.push(buildArray(s.tk,s.stk,s.pos,close)); s.stk.clear }
  protected final def abortStruct(close:Int):Unit               = stackS.pop
  protected final def abortArray(close:Int):Unit                = stackS.pop
  protected final def abortList(close:Int):Unit                 = { }
  protected final def end = if (stackS.size!=1) throw new IllegalStateException("Unclosed Structures in the data source") else buildRoot(stackS.top.stk,stackS.top.pos,tokenizer.position)
}