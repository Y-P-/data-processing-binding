package loader.context

import java.net.URI
import loader._
import loader.annotations._
import loader.core.context._
import scala.collection.mutable.HashMap
import java.io.File


class ConfigContext(tagMapBuilder: =>TagMap = new RegexTagMap) extends Context(tagMapBuilder) {
  //creates a context by reading a file from the given URI 
  def this(tagMapBuilder: =>TagMap, uri:URI) = { this(tagMapBuilder); tagManager.load(uri) }
  def this(tagMapBuilder: =>TagMap, f:File,mode:String) = this(tagMapBuilder,ConfigContext.withMode(f,mode,"UTF-8"))
  def this(tagMapBuilder: =>TagMap, f:File,mode:String,encoding:String) = this(tagMapBuilder,ConfigContext.withMode(f,mode,encoding))
  def this(tagMapBuilder: =>TagMap, f:String,mode:String) = this(tagMapBuilder,ConfigContext.withMode(new File(f),mode,"UTF-8"))
  def this(tagMapBuilder: =>TagMap, f:String,mode:String,encoding:String) = this(tagMapBuilder,ConfigContext.withMode(new File(f),mode,encoding:String))
  def this(uri:URI) = { this(new RegexTagMap); tagManager.load(uri) }
  def this(f:File,mode:String) = this(new RegexTagMap,ConfigContext.withMode(f,mode,"UTF-8"))
  def this(f:File,mode:String,encoding:String) = this(new RegexTagMap,ConfigContext.withMode(f,mode,encoding))
  def this(f:String,mode:String) = this(new RegexTagMap,ConfigContext.withMode(new File(f),mode,"UTF-8"))
  def this(f:String,mode:String,encoding:String) = this(new RegexTagMap,ConfigContext.withMode(new File(f),mode,encoding:String))
  //updates a context by adding the definitions in the given URI
  def update(uri:URI):Unit = tagManager.load(uri)
  def update(f:File,mode:String):Unit = update(ConfigContext.withMode(f,mode,"UTF-8"))
  def update(f:File,mode:String,encoding:String):Unit = update(ConfigContext.withMode(f,mode,encoding:String))
  def update(f:String,mode:String):Unit = update(ConfigContext.withMode(new File(f),mode,"UTF-8"))
  def update(f:String,mode:String,encoding:String):Unit = update(ConfigContext.withMode(new File(f),mode,encoding:String))
  
  //adds a new definition
  def +=(stc:ConfigContext.Struct)  = tagManager.map.put(stc.id,stc)
  def put(stc:ConfigContext.Struct) = tagManager.map.put(stc.id,stc)

  object tagManager extends TagManager {
    import ConfigContext._
    
    val map = new scala.collection.mutable.HashMap[String,Struct]

    def apply(id: String):(StructAnnot,Traversable[FieldAnnot]) = {
      val stc = map.getOrElse(id,null)  //XXX what if doesn't exist ?
      if (stc==null) println(s"$id could not be found")
      (stc,stc.fields)
    }
    
    def load(ctxUri:URI):Unit = ()
/*XXX restore
    /** Loads a new definition context */
    def load(ctxUri:URI)(implicit factory:Factory):Unit = {
      val loader = factory.URILoaders(ctxUri)                //reads URI
      val pos    = new Root(ClassContext(classOf[TagDefs]))  //position to analyze against TagDefs based on Annotations
      val ctx    = new UserContext(pos,true)                 //parser settings
      val exec   = loader.build(pos,ctx,null)                //build executor
      val data   = exec.asObj(new TagDefs)                   //run to build CfgData
      map ++= data.struct                                    //put the defs in the map of known structures
    }*/
  }
}

object ConfigContext {
  
  //utility to create an URI with the file type in the query part
  def withMode(f:File,mode:String,encoding:String) = new URI(s"${f.toURI}?mode=$mode;encoding=$encoding")
  
  trait Struct extends StructAnnot {
    def id:String
    def fields:Traversable[FieldAnnot]
  }
  
  /** Reader for TagField definitions
   *  Note that even though fields are mutable, external code cannot access them due to various private accessor restrictions.
   */
  final private class TagFldHelper extends FieldAnnot with Named { //XXX restore
    var name:String = _
    @TagField var inName         = ""
    @TagField var outName        = ""
    @TagField var qName          = null
    @TagField var loader         = ""
    @TagField var tagEnd         = ""
    @TagField var min            = 0
    @TagField var max            = 0
    @TagField var check          = ""
    @TagField var valid          = ""
    @TagField var param          = ""
    @TagField var audit          = ""
    @TagField var contiguous     = false
    @TagField var isList         = false
    @TagField var isSeq          = false
    @TagField var isFld          = false
    @TagField var rank           = 0
    @TagEnd def end = {
      if (inName.isEmpty) inName=name
      if (!isList && !isSeq) max=1
      this
    }
  }
  
  final private class TagStcHelper extends Struct {
    @TagField                 var auditMin                    = 0
    @TagField                 var auditMax                    = 5
    @TagField                 var fast                        = true
    @TagField                 var id:String                   = ""
    @TagField(inName="class") var clzzz:String                = ""
    @TagSeq(inName="{.*}")    var fieldsx:Array[TagFldHelper] = _
    final def fields:Traversable[FieldAnnot] = fieldsx
    lazy val clzz:Class[_<:AnyRef] = { //use @class as the corresponding class, or by default try the id
      Class.forName(if (!clzzz.isEmpty) clzzz else id).asSubclass(classOf[AnyRef])
    }
    @TagEnd def tagEnd = new Assoc(id,this)
  }
  final private class TagDefs {
    @TagSeq var struct:HashMap[String,TagStcHelper] = _
  }
}