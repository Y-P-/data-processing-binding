package loader.core.names

import loader.core.definition.Processor
import java.util.regex.Pattern

/** Names often come as a pair of namespace/name (e.g. xml)
 *  This lets define the scheme by which a name is separed between prefix and local name.
 *  Note that this is not xml, and the prefix is not tied to any URI.
 *  It may be used for superficial handling of names in xml, though.
 *  The isAttrib method is used to work with xml data ; it provides a way to handle non xml
 *  data in a similar way to xml when it comes to attributes (which are basically non repetitive
 *  fields that come first and whose order is usually irrelevant.)
 *  @param isAttrib, true if the name represents an attribute (xml)
 *  @param prefix, null for no prefix and not empty otherwise
 *  @param local, local name, never null and usually not empty unless the data supports anonymous fields
 */
class QName(val local:String,val prefix:String=null,val isAttrib:Boolean=false)

object QName {

  /** Defines the default pattern for attrib/prefix/local, which is: &@prefix!local
   *  It can be used for interchangeable outName definitions in context
   */
  private val p = Pattern.compile("&(@)?(?:([^!]*)!)(.*)")
  def apply(outName:String):QName = {
    val m = p.matcher(outName)
    if (m.matches()) new QName(m.group(3),m.group(2),m.group(1)!=null)
    else             null
  }
  
  trait Builder extends (Processor#Element=>QName)
  
  //The following classes are defined for use in Contexts.

  /** the default processor for contexts */
  final class NoProc extends Builder { def apply(e:Processor#Element):QName = null }
  final val noProc = new NoProc   //could get rid of this if I knew how to make getClass[object.type]
  /** builds a qname containing only a local part: if nom is '!' then e.name is used, otherwise nom is used */
  final object Local {
    def apply(nom:String):Processor#Element=>QName = if (nom=="!") e=>new QName(e.name) else e=>new QName(nom)
  }
  /** builds a qname from nom (e.name is ignored) or e.name (if nom is null); first by applying the generic pattern above, then using the parser's one */
  final object Const {
    def apply(nom:String):Processor#Element=>QName = if (nom==null) e=>e.parser.qName(e.name) else { val q=QName(nom); if (q==null) _.parser.qName(nom) else e=>q }
  }
  
  //The next methods defines the standard way to build the input QName.
  //The output QName will often be the same, but this depends on the UserContext
  //which may even bypass this whole method.
  
  /** builds the input QName for an element.
   *  this algorithm checks for a contextual processor if any to apply it.
   *  if this fails, it uses the parser's defined qname analyzer.
   */
  def apply(e:Processor#Element):QName = {
    val q = e.proc match { //check for contextual processor if any; null if not found or NoProc
      case e1:loader.core.CtxCore.Processor#Elt => //note: Element<:Elt, but Element is erased in the projection!
        val f = e1.fd.annot.qName   //fetch the contextual processor if any; 
        if (f==null) null else f(e) //if present, use it
      case _ =>
        null
      }
    if (q==null) e.parser.qName(e.name) else q //by default, use parser processor
  }
  
}