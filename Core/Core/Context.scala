package loader.core

import scala.collection.mutable.HashMap
import utils.RegexMap
import XmlHelper.t


/** This class lists all relevant information pertaining to a given 'structure'.
 *  It provides support for reading 'structure' information from varied sources.
 *  @param the unique 'name' (in the context used) of the relevant structure
 *  @return the appropriate sequence of annotations : ('struct annotation', Traversable('field annotation'))
 */
abstract class TagManager extends ((String)=>(StructAnnot,Traversable[FieldAnnot]))

/** A Context is a class that lets build FieldMappings from some kind of source.
 *  It must implement:
 *  - tagManager, which specifies how the tags are read in this context
 */
abstract class Context {
  type Element = definition.Def#Elt
  val tagManager:TagManager
  
  protected def top(id:String,name0:String) = new FieldAnnot {
    val name       = name0
    val kind       = ""
    val tag        = name0
    def loader     = id
    def min        = 1
    def max        = 1
    def regex      = ""
    def valid      = ""
    def param      = ""
    def fmtout     = ""
    def fmtin      = ""
    def contiguous = false  
    def isSeq      = false
    def isList     = false
    def isFld      = false
    def order      = -1
    def tagEnd     = null
  }
  final def apply(id:String):FieldMapping = apply(id,"")
  final def apply(id:String,name:String):FieldMapping = new FieldMapping(top(id,name))
  
  /** Recovers a StructMapping for a given id.
   */
  final def build(id:String):StructMapping = if (id==null || id.isEmpty) null else StructMapping(id)
  
  /** A class that regroups all information about a given mapping.
   */
  final class StructMapping private (
    val id:String,        //the identifier for the mapping ; e.g. the underlying class name for mappings based on reflexion/annotations.
    val annot:StructAnnot,
    val fields:TagMap) {
    final def ctx:Context = Context.this
    def asXml(tab:Int):String = {
      import XmlHelper.t
      s"${t(tab)}<struct ${annot.asXmlAttrib(id)}>${fields.asXml(tab+1)}${t(tab)}</struct>"
    }
  }
  object StructMapping {
    //a cache for the defined StructMapping in this Context ; these mappings are usually expensive to build, and can be reused.
    private val map = new scala.collection.mutable.HashMap[String,StructMapping]
    /** Recovers the mapping for id */
    def apply(id:String):StructMapping = map.get(id) match {
      case None   => tagManager(id) match {
        case null => null
        case x    => val s = build(id,x)
                     map.put(id,s)
                     s
      }
      case Some(s) => s
    }
    /** Pushes the mapping for id as defined in info */
    def push(s:StructMapping):Unit = if (!map.contains(s.id)) map.put(s.id,s)
    def push(id:String,info:(StructAnnot,Traversable[FieldAnnot])):Unit = push(build(id,info))
    
    /** Pushes the mapping for id as defined in info */
    def build(id:String,info:(StructAnnot,Traversable[FieldAnnot])):StructMapping = {
      val tags = new TagMap(info._2.map {u=>(u.tag,new FieldMapping(u))})
      new StructMapping(id,info._1,tags)
    }
    //dump of all known mappings in XML format
    def asXml(tab:Int):String = {
      import XmlHelper.t
      val cr = t(tab+1)
      s"${t(tab)}${map.values.foldLeft("<_>")((x,y)=>x+cr+y.asXml(tab+1))+s"${t(tab)}</_>"}"
    }
    override def toString = asXml(0)
  }
  object FldStatus extends Enumeration {
    class FldStatus extends Val
    val unknown,struct,list,terminal=new FldStatus
  }
  /** A trait that regroups all information about a given field mapping.
   *  Note that this has no relationship to objects fields/methods, even though we may get this information by reading from such items.
   *  When we have to connect to such an item, we have to use the binding method.
   */
  final class FieldMapping(val annot:FieldAnnot) {
    final lazy val loader = if (!annot.isDyn) build(annot.loader) else null
    final def isStruct = loader!=null
    final def isList   = annot.isList
    final def isSeq    = annot.isSeq
    final def name     = annot.name
    final def ctx      = Context.this
    //transforms this list Mapping into a seq mapping ; this is used to manage the internal sequence forming the actual list.
    final def asSeq:FieldMapping = {
      if (!annot.isList) throw new IllegalStateException
      new FieldMapping(new FieldAnnot {
        def name:String = ""
        def kind:String = ""
        def tag:String = ""
        def min:Int = annot.min
        def max:Int = annot.max
        def regex:String = annot.regex
        def valid:String = annot.valid
        def param:String = annot.param
        def fmtout:String = annot.fmtout
        def fmtin:String = annot.fmtin
        def contiguous:Boolean = true
        def isList:Boolean = false
        def isSeq:Boolean = true
        def isFld:Boolean = false
        def loader:String = annot.loader
        def tagEnd:String = annot.tagEnd
        def order:Int = annot.order        
      })
    }
    override def toString = annot.toString
  }
}

/** Managing tags.
 *  In particular, regex tags must be handled in a separate way.
 */
final protected class TagMap extends Traversable[(String,Context#FieldMapping)] { self=>
  private var flds  = RegexMap(HashMap.empty[String,Context#FieldMapping])  //fields or field/attribute definitions
  private var attrs = RegexMap(HashMap.empty[String,Context#FieldMapping])  //attribute only defintions
  /** indicates sequences are expected */
  lazy val hasSeqs = flds!=null && flds.exists(_._2.annot.isSeq)
  /** indicates that non contiguous sequences are expected */
  lazy val hasNonContig = hasSeqs && (flds.exists ((x:(String,Context#FieldMapping))=>x._2.annot.isSeq && !x._2.annot.contiguous))
  /**
   * The constructor.
   * The input map matches the canonical name (the underlying unique name identifying field/scala setter/bean setter) to
   * the pair (Field/Method,AnnotType) ;
   */
  def this(data:Traversable[(String,Context#FieldMapping)]) = {
    this();
    for (x <- data) (if (x._2.annot.attrib.isAttr) attrs else flds).put(x._1,x._2)
  }
  //Attributes can be found in both maps ; so for attribs, first check the attrib map. Attributes may be significantly slower to fetch.
  def fetch(s:String,isAttrib:Boolean) = {
    if (isAttrib) {
      attrs.get(s) match {
        case None => flds.get(s) match {
          case None                                                       => None
          case f:Some[Context#FieldMapping] if (f.get.annot.attrib.isFld) => None //wrong match : this definition is exclusive to fields.
          case f                                                          => f
        }
        case f => f
      }
    } else {
      flds.get(s) //simpler, most common and faster : any match in flds goes for a field. 
    }
  }
  def iterator:Iterator[(String,Context#FieldMapping)] = attrs.iterator ++ flds.iterator
  def foreach[U](f:((String,Context#FieldMapping))=>U) = iterator.foreach(f)
  def values:Iterable[Context#FieldMapping]  = new Iterable[Context#FieldMapping] { def iterator=self.iterator.map(_._2) }
  def asXml(tab:Int):String = s"${(attrs++flds).values.foldLeft("")((x,y)=>x+XmlHelper.t(tab)+y.annot.asXml)}"
}

  
/** A type that describes our field level annotations
 *  Required because Annotations don't have class hierarchies!
 *  And conf files can't ever be Annotations (which is not instanciable in the program)
 *  Acts as a common interface.
 *  Each context must provide a way to recover this info. 
 */
abstract class FieldAnnot {
  def name:String    //the name for the field
  def kind:String    //the name of a partition for that field (think attribututes)
  def tag:String     //the tag(s) actually associated to that field
  def min:Int
  def max:Int
  def regex:String
  def valid:String
  def param:String
  def fmtout:String
  def fmtin:String
  def contiguous:Boolean
  def isList:Boolean
  def isSeq:Boolean
  def isFld:Boolean     //indicates that a field is terminal
  def loader:String     //the id to a StructAnnot ; null if not mapped to a structure ; "" if the structure is unknown (dynamic)
  def tagEnd:String     //the tagEnd method to call from the loader.clzz object to build this field
  def order:Int         //expected processing order ; using this feature may cause a severe performance hit
  final def asXml:String = {
    import XmlHelper._
    s"<$name${if (tag!=name) v(" tag",tag) else ""}${v("min",min,0)}${v("min",max,5)}${v("regex",regex)}${v("valid",valid)}${v("param",param)}${v("fmtout",fmtout)}${v("fmtin",fmtin)}${v("contiguous",contiguous)}${v("isList",isList)}${v("isSeq",isSeq)}${v("isFld",isFld)}${v("loader",loader)}${v("tagEnd",tagEnd)}${v("order",order,-1)}/>"
  }
  def isDyn:Boolean = loader!=null && loader.isEmpty
  override def toString  = s"FieldAnnot<$name>(${if (isList)"lst"else if(isSeq)"seq"else"fld"}${if (loader!=null && !loader.isEmpty())s":$loader"else""})"
}
  
/** A type that describes our struct level annotations
 *  Required because Annotations don't have class hierarchies!
 *  And conf files can't ever be Annotations (which is not instanciable in the program)
 *  Acts as a common interface.
 *  Each context must provide a way to recover this info. 
 */
abstract class StructAnnot {
  def auditMin:Int
  def auditMax:Int
  def fast:Boolean
  def clzz:Class[_<:AnyRef]  //the class attached to this structure in case we want to build an object
  final def asXmlAttrib(id:String):String = {
    s"id='$id' auditMin='$auditMin' auditMax='$auditMax' fast='$fast' ${if(id!=clzz.getName)s" clzz='${clzz.getName}'"else""}"
  }
}

private object XmlHelper {
  val t = Array[String]("\n","\n  ","\n    ","\n      ","\n        ","\n          ","\n            ","\n              ","\n                ","\n                  ","\n                    ")
  def v(nm:String,v:String)  = if (v!=null && v.length>0) s" $nm='$v'"   else ""
  def v(nm:String,v:Boolean) = if (v)                     s" $nm='true'" else ""
  def v(nm:String,v:Int,default:Int) = if (v!=default)    s" $nm='true'" else ""
}

      
