package loader.context
import java.lang.reflect.{Modifier,Field}
import scala.collection.mutable.{Set,HashMap,ListBuffer}
import loader._
import loader.core.context._
import loader.reflect.{Analyze,Converter}
import loader.annotations.TagEnd
import loader.core.exceptions.DynamicInvocation
import java.io.Writer
import loader.annotations.TagField

/** The default implementation uses RegexTagMap
 */
object SerialContext extends ClassContext(new RegexTagMap)

/**
 * Context using global information by watching fields.
 * o Collections are expected as lists
 * o transient fields are ignored
 * o super class must call their own reader : only local fields are read  XXX not true
 * Only one such context can exist.
 * 
 * This context must be used with great care, especially in scala.
 * private fields will be overwritten, including in parent classes, and
 * this may yield unsavory results. You should restrict the use of this
 * utility to simple declarative classes with no logic and basic fields.
 * The result on 'lazy val' is undefined.
 * 
 * This context is severely limited and should only be used for:
 * - pure serialization
 * - very simple objects as defined above
 */
class SerialContext(tagMapBuilder: =>TagMap) extends Context(tagMapBuilder) {

  final def apply(clzz:Class[_<:AnyRef]):FieldMapping = apply(clzz.getName)
  
  /** Dynamic loader used for serialization.
   */
  final private class Serial {
    var clzz:Class[_] = _  //class name of the object to load  XXX problem if using the 'tag' attribute
    var obj:AnyRef    = _  //loaded object
  }
  object Serial {
  }
  
  /*XXX restore
  class SerialUserContext(val obj:AnyRef) extends UserContext(SerialContext(classOf[Serial]),false)
  
  def serialize(o:AnyRef,out:Writer):Unit = {
    val s = new Serial
    s.obj = o
    s.clzz = o.getClass
    ObjectParser().run(new SerialUserContext(o),SerialContext(classOf[Serial]),s).printXml(out)
  }
  */

  /** Tag manager for annotations
   */
  object tagManager extends TagManager {
  
    final private val sortedFields = new HashMap[Class[_],Array[FieldAnnot]]
    
    /** Gets relevant info from field by reflexion:
     *  - tag name
     *  - loader to use if not present
     *  @returns null      if no loader required (converted field)
     *           loader-id for a specific loader
     *           ""        for a dynamically found loader
     */
    def loaderId(f:Field,isList:Boolean):String = {
      try {
        val ana = Analyze(f,null,isList,false)
        val cz=ana.getLoadableClass
        if (cz!=null && classOf[AnyRef].isAssignableFrom(cz) && !Converter.canConvert(cz)) cz.getName else null
      } catch {
        case d:DynamicInvocation =>
          println(s"resolving dynamic invocation on $f which is ${if(!isList)"not "}a list")
          if (Converter.canConvert(f.getType)) null
          else                                 ""
      }
    }
    final private class TagFieldHelper(f:Field) extends FieldAnnot {
      val inName:String      = f.getName
      val outName:String     = f.getName
      def min:Int            = 0
      def max:Int            = 1
      def check:String       = ""
      def valid:String       = ""
      def param:String       = ""
      def audit:String       = ""
      def contiguous:Boolean = false
      def isList:Boolean     = false
      def isSeq:Boolean      = false
      val loader:String      = loaderId(f,false)
      def tagEnd:String      = null
      def isFld:Boolean      = loader==null
    }
    final private class TagListHelper(f:Field) extends FieldAnnot {
      val inName:String      = f.getName
      val outName:String     = f.getName
      def min:Int            = 0
      def max:Int            = 0
      def check:String       = ""
      def valid:String       = ""
      def param:String       = ""
      def audit:String       = ""
      def contiguous:Boolean = true
      def isList:Boolean     = true
      def isSeq:Boolean      = false
      val loader:String      = loaderId(f,true)
      def tagEnd:String      = null
      def isFld:Boolean      = loader==null
    }
    final private class TagMapHelper(f:Field) extends FieldAnnot {
      val inName:String      = f.getName
      val outName:String     = f.getName
      def min:Int            = 0
      def max:Int            = 0
      def check:String       = ""
      def valid:String       = ""
      def param:String       = ""
      def audit:String       = ""
      def contiguous:Boolean = true
      def isList:Boolean     = true
      def isSeq:Boolean      = false
      val loader:String      = classOf[Assoc[_,_]].getName  //map elements are loaded in an Assoc
      def tagEnd:String      = null
      def isFld:Boolean      = loader==null
    }
    final private class TagStructHelper(val clzz:Class[_<:AnyRef]) extends StructAnnot {
      println(s"struct helper for $clzz")
      def auditMin = 0
      def auditMax = 0
      def fast     = true
    }
    private final def annot(f:Field):FieldAnnot = {
      val t = Analyze.checkNamed(f.getGenericType)
      val (isList,isMap) = Analyze.isCollection(t._1)
      if      (isMap)  new TagMapHelper(f)
      else if (isList) new TagListHelper(f)
      else             new TagFieldHelper(f)
    }
    
    protected final def sort(x1:Field,x2:Field):Boolean = x1.getName.compareTo(x2.getName)<0
    protected final def notTransient(x:Field):Boolean   = !Modifier.isTransient(x.getModifiers)
  
    /** Lists all fields for a class, including superclass protected/private ones.
     *  They come in alphabetical order. The sorting is done once, then cached.
     */
    protected final def getFields(cz:Class[_]):Traversable[FieldAnnot] = new Traversable[FieldAnnot] {
      //find sorted fields for that class
      final val flds:Array[FieldAnnot] =
        sortedFields.getOrElseUpdate(cz,cz.getDeclaredFields.filter(notTransient).sortWith(sort).map(annot))
      def foreach[U](g:(FieldAnnot)=>U):Unit = if (cz!=classOf[AnyRef]) {
        getFields(cz.getSuperclass).foreach(g)
        for (f <- flds) g(f)
      }
    }
    
    /** Here, the id is the full class name */
    final def apply(id:String):(StructAnnot,Traversable[FieldAnnot]) =
      if      (id==null) null  //applies to non struct fields
      else               apply(Class.forName(id).asSubclass(classOf[AnyRef]))
  
    final def apply(clzz:Class[_<:AnyRef]):(StructAnnot,Traversable[FieldAnnot]) = {
      if (Converter.canConvert(clzz)) null
      else                            (new TagStructHelper(clzz),getFields(clzz))
    }
  }
/*XXX restore
  //this section declares and stores the known dynamic loaders for that context.
  //at this moment, dynamic loaders cannot be defined by users.
  {
    // loader.Assoc, used to load map items
    new DynLoader {
      def id = classOf[Assoc[_,_]].getName
      def dynClzz(parent:Element,fd:Context#FieldMapping):Class[_] = {
        val cza = parent.binding.paramTypes
        if (fd.name=="key") cza(0) else cza(cza.length-1) //key is first generic param, value is last
      }
    }
    // loader.context.SerialContext$Serial, used for serialization
    new DynLoader {
      def id = classOf[Serial].getName
      def dynClzz(parent:Element,fd:Context#FieldMapping):Class[_] = { //will be called for obj field
        val cz = parent.stk.ctx.asInstanceOf[SerialUserContext].obj.getClass
        println(s"called dynamic resolution for ${fd.name} ; found $cz")
        //loaded class is stored in the parent Serial item being built
        cz
        //parent.motor.asInstanceOf[BaseObjectEngine].item.asInstanceOf[Serial].clzz
      }
    }
  }
  */
}

