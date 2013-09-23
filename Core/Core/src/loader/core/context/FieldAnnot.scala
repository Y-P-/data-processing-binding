package loader.core.context

/** A type that describes our field transformation.
 *  Each context must provide a way to recover this info.
 *  Such info is always tightly bound to the triplet (parser/auditer/processor) ; possibly including whatever callbacks you use.
 *  
 *  The transformation almost always involves reading a field (inname), processing it (param) and writing it (outname)
 *  These usually only have a meaning in the context of the parser (inname)/processor (param/outname)
 *  
 *  We also add information for auditing, since this is a frequent requirement. Of course, this also ties
 *  the info to the kind of auditer used.
 */
abstract class FieldAnnot {
  def inName:String  //the name for the field in the parser ; the name maybe in the form namespace:name, whatever namespace means.
  def outName:String //the name for the field for the processor ; the name maybe in the form namespace:name, whatever namespace means.
  def loader:String  //the id to a StructAnnot ; null if not mapped to a structure ; "" if the structure is unknown (dynamic)
  def isList:Boolean //indicates that the fields is a list: it contains a unique field sequence
  def isSeq:Boolean  //indicates that the field can be repeated
  def isFld:Boolean  //indicates that a field is terminal
  def isStc:Boolean = !isFld && !isList
  
  /* information pertaining to the expected syntax.
   */
  def contiguous:Boolean //applies to sequences only ; true if the element of the sequence should follow each other (no interleave)
  def min:Int            //min number of excepted occurences
  def max:Int            //max number of expected occurences
  
  /* next come generic info that most Context/Processor/Auditer might use.
   * filling up these is context dependant.
   */
  def audit:String //info pertaining to the element for auditing  
  def check:String //info for checking the validity of the parser data (Parser.Kind)
  def valid:String //info for checking the validity of the processor data (Processor.Kind)
  def param:String //info for transforming the data within the processor
  
  final def asXml:String = {
    import XmlHelper._
    s"<$inName${if (outName!=inName) v(" outName",outName) else ""}${v("min",min,0)}${v("min",max,5)}${v("contiguous",contiguous)}${v("audit",audit)}${v("check",check)}${v("valid",valid)}${v("param",param)}${v("isList",isList)}${v("isSeq",isSeq)}${v("isFld",isFld)}${v("loader",loader)}/>"
  }
  override def toString  = s"FieldAnnot<$inName>(${if (isList)"lst"else if(isSeq)"seq"else"fld"}${if (loader!=null && !loader.isEmpty())s":$loader"else""})"
}