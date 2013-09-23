package parser.px.analyzers.structured.predefs
import scala.collection.SeqProxy
import scala.collection.mutable.{BufferProxy,Buffer,ArrayBuffer}
import scala.collection.immutable.Stack
import parser.Source
import parser.px.Tokenizer
import parser.px.analyzers.structured._
import parser.px.analyzers.structured.{ Field => BField }
import scala.collection.mutable.ListBuffer
import parser.px.Analyzer

/**
 * Another Field implementation.
 * This time, the focus is not on simplicity and generality, but on the ability
 * to manipulate (i.e. edit) the source file.
 * A secondary purpose is to keep the actual data in the source array rather than
 * copying it.
 * 
 * For these reasons, we have to keep references to the positions of items in the
 * the source.
 * This class should probably not be used unless you wish to effectively alter the
 * content of an existing file while keeping a maximum of information in that file
 * (i.e. comments) and/or you don't want to load the whole file in memory.
 * In effect, this class can let you, for exemple, load a single field in the data
 * file (likely using filters), edit or delete that field, and rewrite the whole
 * modified file, even though you haven't loaded any information about the missing
 * data.
 * 
 * Achieving this comes at some price ; most notably, you must limit yourself to
 * simple modifications ; this means not pruning some parts of the tree elsewhere,
 * not using another tree of this same kind as source of data. These are reasonable
 * limitations.
 * 
 * You must also be aware that deleted fields remain in the list of fields, unless
 * they are from another source (so why did you add them in the first place ?)
 * The deleted field tells you whether a given field is deleted or not. 
 * 
 * This class should probably not be used unless you know what you do ; it's behavior
 * can be confusing if you try complex things.
 * Still, it can achieve some usefull things, and having a second implementation
 * of fields is a good exercise to make streamlined Field/Container (etc.) traits,
 * and to test them. It is also good tutorial material.
 */


trait CompactField extends FieldDef { this:Analyzer =>   //Data will have to come from an analyzer. Don't play the fool.
  protected type Fld = Field
  protected type TKN = (Int,Int)
  protected type Dl  = DataField
  protected type DE  = StructField
  protected type DSN = StructField
  protected type DSA = ArrayField
  protected type DSR = RootField
  protected type DF  = BaseField
  protected type DA  = StructField
  protected type DL  = StructField
  
  protected def buildName(name:Tk):TKN = (name.pos,name.length)
  protected def buildField(name:Tk,data:Tk)                            = new BaseField(name.pos,name.length.asInstanceOf[Short],data.pos,data.length.asInstanceOf[Short])
  protected def buildList(name:TKN,vals:Seq[Dl],open:Int,close:Int)    = new StructField(name._1,name._2.asInstanceOf[Short],open,close,vals)
  protected def buildStruct(name:TKN,fld:Seq[Fld],open:Int,close:Int)  = new StructField(name._1,name._2.asInstanceOf[Short],open,close,fld)
  protected def buildStruct(fld:Seq[Fld],open:Int,close:Int)           = new ArrayField(open,close,fld)
  protected def buildRoot(fld:Seq[Fld],open:Int,close:Int)             = new RootField(fld)
  protected def buildArray(name:TKN,fld:Seq[Fld],open:Int,close:Int)   = new StructField(name._1,name._2.asInstanceOf[Short],open,close,fld)
  protected def buildEmpty(name:Tk,open:Int,close:Int)                 = new StructField(name.pos,name.length.asInstanceOf[Short],open,close,null)
  protected def buildListElt(data:Tk)                                  = new DataField(data.pos,data.length.asInstanceOf[Short])

  //make direct access for these which we will constantly refer to
  protected val data     = tokenizer.data
  protected val encoding = tokenizer.src.encoding

  protected trait Field extends parser.px.analyzers.structured.Field {
    var deleted:Boolean = false     // indicates if the field has been deleted
    protected def beg:Int           // first char for the whole field
    protected def end:Int           // last char for the whole field
    
    /**
     * Similar to printer.
     * However, it uses the data source for everything to output, except edited/new/deleted fields.
     * If nothing has been changed, it will exactly copy src.data.
     * If a field has been edited, it will copy src.data up to that field value position, then print the new value,
     * then copy src.data from that field value end position.
     * If a field has been deleted, it will copy src.data up to that field start position, then copy src.data from
     * that field end position.
     * If a field is brand new, it will copy src.data up to the previous field end position, then it will print the
     * new field, then it will copy src.data up from the previous field end position.
     * Multiple changes are handled in a similar fashion.
     * 
     * Partial implementation for deleted fields.
     * Returns : the new "last" pointer, equal to the entering last if nothing was done.
     */
    def printer(use:(Array[Byte],Int,Int)=>Unit):Unit = printer2(new Flusher(use) { last=beg-1 } )
    
    def printer2(f:Flusher):Unit = if (deleted) { f.flush(beg-1); f.set(end) }
  }
  
  protected trait Named extends Field with parser.px.analyzers.structured.Named {
    val nmPos:Int       //name position
    val nmLength:Short  //name length
    val name = new String(data,nmPos,nmLength,encoding)
    final def beg=nmPos
  }
  protected trait Anonymous extends Field with parser.px.analyzers.structured.Anonymous {
    final def beg = this match {
      case f:Leaf      => f.dtPos
      case f:Container => f.open
    }
  }
  protected trait Leaf extends Field with parser.px.analyzers.structured.Leaf {
    val dtPos:Int          //data position
    val dtLength:Short     //data length
    var edited:String=null //edited value if any
    final def end=dtPos+dtLength
    override def value = if (edited==null) super.value else edited
    override def :=(newValue:String):Unit = edited = newValue
    lazy val valueAsStr = new String(data,dtPos,dtLength,encoding)
    abstract override def printer2(f:Flusher):Unit = {
      if (deleted) {
        super.printer2(f)
      } else if (edited!=null) {
        f.flush(dtPos-1)
        f.useStr(edited)
        f.set(end-1)
      } else
        f.move(end-1)
    }
  }
  protected trait Container extends Field with SeqProxy[BField] with parser.px.analyzers.structured.Container {
    val open:Int  //open brace position
    val close:Int //close brace position
    final def end=close
    final override def :=(newValues:BField*):Unit = {
      self --= self.iterator.filter(!_.isInstanceOf[Field])
      foreach { _.asInstanceOf[Field].deleted=true }
      self++=newValues
    }
    override def insertAt(old:BField,fld:BField):Unit    = self.insert(find(old),fld)
    override def insertAfter(old:BField,fld:BField):Unit = self.insert(find(old)+1,fld)
    override def replace(old:BField,fld:BField):Unit     = {
      self(find(old)) match {
        case f:Field => self.insert(find(old)+1,f); f.deleted=true
        case f       => f := fld
      }
    }
    override def delete(old:BField):Unit = old match {
        case f:Field  => f.deleted = true
        case f:BField => self.remove(find(old))
      }
    val self:ArrayBuffer[BField] = new ArrayBuffer[BField]
    protected def init(fld:Seq[BField]=null) = if(fld!=null) { self.sizeHint(fld.length); self++=fld }
    abstract override def printer2(f:Flusher):Unit = {
      if (deleted) {
        super.printer2(f)
      } else {
        f.move(open)
        foreach { _ match {
            case fld:Field => fld.printer2(f)       //fields of this kind : print
            case fld       => f.flush               //inserted fields from other places : flush all current
                              fld.printer(f.useStr) //print external Field
                              f.useStr(" ")         //ensure at least one space between fields
          }
        }
        f.move(end)
      }
    }
  }
  
  protected trait Root extends Field with parser.px.analyzers.structured.Root {
    abstract override def printer2(f:Flusher):Unit = {
      super.printer2(f)
      f.flush(data.length-1)
    }    
  }

  class BaseField(val nmPos:Int,val nmLength:Short,val dtPos:Int,val dtLength:Short)             extends Leaf      with Named
  class DataField(val dtPos:Int,val dtLength:Short)                                              extends Leaf      with Anonymous
  class StructField(val nmPos:Int,val nmLength:Short,val open:Int,val close:Int,fld:Seq[BField]) extends Container with Named               { init(fld) }
  class ArrayField(val open:Int,val close:Int,fld:Seq[BField])                                   extends Container with Anonymous           { init(fld) }
  class RootField(fld:Seq[BField])                                                               extends Container with Root with Anonymous { val open=0; val close=data.length-1; init(fld) }

  protected class Flusher(val use:(Array[Byte],Int,Int)=>Unit) {
    protected var last = -1 //last index in src that was updated
    protected var cur  = -1 //current index in src up to which to update (included)
    def flush(upTo:Int)  = if (upTo>last) { use(data,last+1,upTo-last); set(upTo) }
    def flush():Unit     = flush(cur)
    def set(idx:Int)     = { last=idx; cur=idx }
    def move(idx:Int)    = { cur=idx }
    def useStr(s:String) = { val b = s.getBytes(encoding); use(b,0,b.length) }
  }

}

