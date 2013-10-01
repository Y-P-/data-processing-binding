package loader.core.context

import loader.core.names.QName


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
abstract class Context(tagMapBuilder: =>TagMap) {
  type Element = loader.core.definition.Def#Elt
  val tagManager:TagManager
  
  protected def top(id:String,name0:String) = new FieldAnnot {
    val inName     = name0
    val loader     = id
    def rank       = 0
    def qName      = QName.noProc
    def isSeq      = false
    def isList     = false
    def isFld      = false
    def check      = ""
    def valid      = ""
    def param      = ""
    def audit      = ""
    def min        = 1
    def max        = 1
    def contiguous = false  
  }
  final def apply(id:String):FieldMapping = apply(id,"")
  final def apply(id:String,inName:String):FieldMapping = new FieldMapping(top(id,inName))
  
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
      val tags = tagMapBuilder
      for (x <- info._2.map {u=>(u.inName,new FieldMapping(u))}) tags.put(x)
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
  /** A class that groups all information about a given field mapping.
   *  Note that this has no relationship to objects fields/methods, even though we may get this information by reading from such items.
   *  When we have to connect to such an item, we have to use the binding method.
   */
  final class FieldMapping(val annot:FieldAnnot) {
    final lazy val loader = build(annot.loader)
    final def isStruct = loader!=null
    final def isList   = annot.isList
    final def isSeq    = annot.isSeq
    final def inName   = annot.inName
    final def ctx      = Context.this
    //transforms this list Mapping into a seq mapping ; this is used to manage the internal sequence forming the actual list.
    final def asSeq:FieldMapping = {
      if (!annot.isList) throw new IllegalStateException
      new FieldMapping(new FieldAnnot {
        def inName:String  = ""
        def rank:Int       = annot.rank
        def qName          = QName.noProc
        def min:Int        = annot.min
        def max:Int        = annot.max
        def check:String   = annot.check
        def valid:String   = annot.valid
        def param:String   = annot.param
        def audit:String   = annot.audit
        def contiguous:Boolean = true
        def isList:Boolean = false
        def isSeq:Boolean = true
        def isFld:Boolean = false
        def loader:String = annot.loader
        def order:Int = 0        
      })
    }
    override def toString = annot.toString
  }
}