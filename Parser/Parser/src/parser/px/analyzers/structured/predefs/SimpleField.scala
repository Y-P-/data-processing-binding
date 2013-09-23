package parser.px.analyzers.structured.predefs


import scala.collection.SeqProxy
import scala.collection.mutable.{BufferProxy,ListBuffer}
import parser.px.Tokenizer
import parser.px.analyzers.structured._

/**
 * Defines recursive structures able to accept data from a PX file format.
 * These structure use a common type (TKN) to store the information coming out of the analyzer.
 * As space goes, this implementation is not cheap. However, it does it's job well and easily.
 * 
 * Note that unlike the CompactField implementation, this one comes with no strings attached.
 * You can implement a FieldDef that's not dependent on a Analyzer.
 * This is actually what we do with the Builder class when we build fields out of "thin air",
 * making a DSL rather than analyzing a file. 
 */
trait SimpleField extends FieldDef {
  protected type Tk <: Tokenizer#Token
  
  protected type Fld = Field
  protected type TKN = String
  protected type Dl  = DataField
  protected type DE  = StructField
  protected type DSN = StructField
  protected type DSA = ArrayField
  protected type DSR = RootField
  protected type DF  = BaseField
  protected type DA  = StructField
  protected type DL  = StructField
  
  protected def buildField(name:Tk,data:Tk)                            = new BaseField(buildName(name),data.baseString)
  protected def buildList(name:TKN,vals:Seq[Dl],open:Int,close:Int)    = new StructField(name,vals)
  protected def buildStruct(name:TKN,fld:Seq[Field],open:Int,close:Int)= new StructField(name,fld)
  protected def buildStruct(fld:Seq[Field],open:Int,close:Int)         = new ArrayField(fld)
  protected def buildRoot(fld:Seq[Field],open:Int,close:Int)           = new RootField(fld)
  protected def buildArray(name:TKN,fld:Seq[Field],open:Int,close:Int) = new StructField(name,fld)
  protected def buildEmpty(name:Tk,open:Int,close:Int)                 = new StructField(buildName(name),null)
  protected def buildListElt(data:Tk)                                  = new DataField(data.baseString)
  protected def buildName(t:Tk)                                        = t.baseString
  
  protected trait Leaf extends parser.px.analyzers.structured.Leaf {
    override def :=(newValue:String):Nothing = throw new UnsupportedOperationException("simple fields don't support edition")    
  }
  
  protected trait Container extends SeqProxy[Field] with parser.px.analyzers.structured.Container {
    val self:ListBuffer[Field]=new ListBuffer[Field]
    override def :=(newValues:Field*)                  = { self.clear; self++=newValues }
    override def insertAt(old:Field,fld:Field):Unit    = self.insert(find(old),fld)
    override def insertAfter(old:Field,fld:Field):Unit = self.insert(find(old)+1,fld)
    override def replace(old:Field,fld:Field):Unit     = self(find(old)) = fld
    override def delete(old:Field):Unit                = self.remove(find(old))
    protected def init(fld:Seq[Field])                 = if (fld!=null) self++=fld 
  }
  
  class BaseField(val name:String, val valueAsStr:String)      extends Leaf      with Named
  class DataField(val valueAsStr:String)                       extends Leaf      with Anonymous
  class StructField(val name:String,fld:Seq[Field])            extends Container with Named               { init(fld) }
  class ArrayField(fld:Seq[Field])                             extends Container with Anonymous           { init(fld) }
  class RootField(fld:Seq[Field])                              extends Container with Root with Anonymous { init(fld) }
  
  class Builder(name:String) extends parser.px.analyzers.structured.Builder(name) {
    override def :=(value:String):Field with Leaf = {
      if (name.length==0) new DataField(value)
      else                new BaseField(name,value)
    }
    override def :=(values:Field*):Fld with Container = {
      if (values.length==0)    new StructField(name,values)
      else if (name.length==0) new ArrayField(values)
      else                     new StructField(name,values)
    }
  }
}

object SimpleField {
  import scala.language.implicitConversions
  protected object base extends SimpleField
  implicit def toBuilder(name:String) = new base.Builder(name)
}