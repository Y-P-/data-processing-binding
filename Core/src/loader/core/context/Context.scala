package loader.core.context


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
  type Element = loader.core.definition.Processor#Elt
  val tagManager:TagManager
  
  protected def top(id:String,name0:String) = new FieldAnnot {
    val inName     = name0
    val loader     = id
    def rank       = 0
    def isSeq      = false
    def isList     = false
    def isFld      = false
    def depth      = 0
    def check      = ""
    def valid      = ""
    def param      = ""
    def audit      = ""
    def min        = 1
    def max        = 1
    def contiguous = false  
    def convert    = ""
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
  
  /** A class that groups all information about a given field mapping.
   *  Note that this has no relationship to objects fields/methods, even though we may get this information by reading from such items.
   *  When we have to connect to such an item, we have to use the binding method.
   */
  final class FieldMapping(val annot:FieldAnnot) extends utils.reflect.AutoConvertData {
    final val ctx:Context.this.type = Context.this
    final lazy val loader = build(annot.loader)
    final def isStruct = loader!=null
    final def isList   = depth > 0
    final def isSeq    = annot.isSeq
    final def depth    = annot.depth
    final def inName   = annot.inName
    final def check    = annot.check
    final def param    = annot.param
    final def valid    = annot.valid
    final def convert  = annot.convert
    //transforms this list/seq Mapping into a seq mapping ; this is used to manage the internal sequence forming the actual list.
    final def asSeq:FieldMapping = {
      if (annot.depth<=0) throw new IllegalStateException("it is not legal to use asSeq on a FieldMapping that is not a list/sequence")
      new FieldMapping(new FieldAnnot {
        def inName:String  = ""
        def min:Int        = annot.min
        def max:Int        = annot.max
        def check:String   = annot.check
        def valid:String   = annot.valid
        def param:String   = annot.param
        def audit:String   = annot.audit
        def contiguous:Boolean = true
        def isSeq:Boolean  = true
        def isList:Boolean = depth > 0
        def depth:Int      = annot.depth-1
        def loader:String  = annot.loader
        def convert:String = annot.convert     
      })
    }
    /** Rebuilds a fd using a different depth and loader.
     *  This is usually called when the initial annot was minimal (left for inference.)
     */
    final def rebuild(loader0:String, isSeq0:Boolean, depth0:Int):FieldMapping = {
      new FieldMapping(new FieldAnnot {
        def inName:String      = annot.inName
        def loader:String      = loader0
        def isSeq:Boolean      = isSeq0
        def depth:Int          = depth0 - (if (isSeq) 1 else 0)
        def isList:Boolean     = depth > 0
        def contiguous:Boolean = annot.contiguous
        def min:Int            = annot.min
        def max:Int            = annot.max
        def audit:String       = annot.audit
        def check:String       = annot.check
        def valid:String       = annot.valid
        def param:String       = annot.param
        def convert:String     = annot.convert
      })
    }
    /** Rebuilds a fd using a different depth.
     *  This is usually called when the initial annot depth was -1 (i.e. left for inference.)
     */
    final def rebuild(depth0:Int):FieldMapping = rebuild(annot.loader,annot.isSeq,depth0)

    override def toString = annot.toString
  }
}