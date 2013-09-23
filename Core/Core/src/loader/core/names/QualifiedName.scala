package loader.core.names

/** Names often come as a pair of namespace/name (e.g. xml)
 *  This lets define the scheme by which a name is separed between prefix and local name.
 *  Note that this is not xml, and the prefix is not tied to any URI.
 *  It may be used for superficial handling of names in xml, though.
 *  The isAttrib method is used to work with xml data ; it provides a way to handle non xml
 *  data in a similar way to xml when it comes to attributes (which are basically non repetitive
 *  fields that come first and whose order is usually irrelevant.)
 */
abstract class QualifiedName(val name:String) {
  def isAttrib:Boolean  //true if the name represents an attribute
  def prefix:String     //null for no prefix and not empty otherwise
  def local:String      //local name, never null and usually not empty unless the data supports anonymous fields
}

object QualifiedName {
  /** A scheme is the way through which a name is deassembled from the source input.
   *  There usually are
   *  - two QualifiedName: the input QualifiedName and the output QualifiedName.
   *  - one Converter that builds the output QualifiedName from the input QualifiedName.
   *  - one Scheme that deassembles the original name into a QualifiedName
   *  Combining the input Scheme with a Converter yields a 'final' Scheme that build
   *  the output QualifiedName from the parsed input name.
   */
  type Scheme    = String => QualifiedName
  type Converter = QualifiedName => QualifiedName
  
  /** A full scheme that analyzes the input and converts it for output */
  def apply(sc:Scheme,cv:Converter):Scheme = x => cv(sc(x))
  /** A basic scheme that does nothing but pass the input unchanged */
  final val basic:Scheme = new QualifiedName(_) { val isAttrib=false; val prefix=null; def local=name; }
  
}