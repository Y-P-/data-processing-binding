package loader

import java.lang.reflect.Method
import scala.collection.mutable.HashMap
import utils.RegexMap
import loader.annotations.AttrKind
import loader.reflect.{FldActor,TagEndInvoker,Analyze,Converter,Binder}
import loader.exceptions.{DynamicInvocation,DynamicConstructor}


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
  val tagManager:TagManager
  
  //This attaches an id with a DynLoader
  protected val dynMap = new scala.collection.mutable.HashMap[String,DynLoader]
  
  /** This lets record a class for dynamic loading.
   *  Simply declaring a val for a loader loads it. 
   *  XXX likely obsolete
   */
  trait DynLoader {
    def id:String                                                 //id for that class = class name, whatever the context is
    def dynClzz(parent:Element,fd:Context#FieldMapping):Class[_]  //appropriate loader class based on current context
    def dyn(parent:Element,fd:Context#FieldMapping):Analyze = Analyze(dynClzz(parent,fd),null,fd.isList,fd.isSeq)
    dynMap.put(id,this)
  }
  
  /** Retrieves a dynamic loader
   *  NOTE: this only gets invoked within a BaseObjectEngine
   */
  final def dynamic(parent:Element,fd:FieldMapping):Analyze  = dynMap(parent.fd.loader.id).dyn(parent,fd)
  final def dynClzz(parent:Element,fd:FieldMapping):Class[_] = dynMap(parent.fd.loader.id).dynClzz(parent,fd)
  
  protected def top(id:String,name0:String) = new FieldAnnot {
    val name       = name0
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
    def attrib     = AttrKind.fld
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
    final def dynamic(parent:Element):Analyze  = Context.this.dynamic(parent,this)
    final def dynClzz(parent:Element):Class[_] = Context.this.dynClzz(parent,this)  //XXX can we use this inside Analyze and not launch an exception ?
    def load(parent:Element) = {                     //returns that actual (FieldMapping, isList, !isFld) ; usually trivial, except when loading dynamically
      val isList = annot.isList && parent.fd!=this  //if parent has same fd, then we are dealing with a list element
      if (loader==null && !annot.isFld) {
        val ana = dynamic(parent)
        val cz  = dynClzz(parent)
        if (ana.getConverter==None) (Context.this(cz.getName,name),isList,true)
        else                        (this,isList,false)
      } else                        (this,isList,!annot.isFld)      
    }
    private var bind:Binder = _                //TODO ? should really be a map. Yet, we have no use case for this at this time and maps are expensive.
    final def binding(child:Element):Binder = { //binds this child loader (which is associated with this FieldMapping) within its hierarchy
      if (child.fd!=this) throw new IllegalStateException
      val parent = child.parentStc.fd.loader      //most often, only this is used, and results in a constant value ; in a multi-binding environment
      if (bind==null) {                        //(where more than one class can be bound to a given mapping), we would have one 'constant' result
        bind = if (parent==null) {             //possible parent (resulting in the todo task above)
          Binder(this)                         //actually bings to nothing
        } else {                               //note that we will cache the result to prevent expensive introspection
          Binder(child)                        //actual binding
        }
      }
      bind
    }
    val valid = Converter.validStr(annot.regex) //a valider that will throw a ParserException if the input doesn't match the annot regex
    //transforms this list Mapping into a seq mapping ; this is used to manage the internal sequence forming the actual list.
    final def asSeq:FieldMapping = {
      if (!annot.isList) throw new IllegalStateException
      new FieldMapping(new FieldAnnot {
        def name:String = ""
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
        def attrib:AttrKind = annot.attrib
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
  def attrib:AttrKind   //indicates that a field is expected to be an attribute
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

      
