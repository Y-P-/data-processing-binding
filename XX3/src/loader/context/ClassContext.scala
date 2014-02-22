package loader.context
import java.lang.annotation.Annotation
import java.lang.reflect.{Method,Field,Modifier,Member,AnnotatedElement}
import scala.collection.mutable.{ListBuffer,HashMap}
import loader.core.context._
import loader.core.exceptions.DynamicInvocation
import loader.core.names.QName
import loader.annotations.{TagField,TagStruct,TagSeq,TagList}
import loader.core.context.RegexTagMap
//XXX import loader.reflect.Analyze

/** The default implementation uses RegexTagMap
 */
object ClassContext extends ClassContext(new RegexTagMap)

/** Context using global information through annotations.
 *  Only one such context can exist.
 */
class ClassContext(tagMapBuilder: =>TagMap) extends Context(tagMapBuilder) {
  
  //This covers fields, methods and constructors
  protected type Annoted = Member with AnnotatedElement

  /** Factory specific to that Context : it builds the id from the Class name.
   */
  def apply(clzz:Class[_<:AnyRef]):FieldMapping = apply(clzz.getName)

  /** A class used to provide a default value for annotations. */
  protected final class Unknown
  
  /** Tag manager for annotations
   */
  object tagManager extends TagManager {
    /** Gets relevant info from field by reflexion:
     *  - tag name
     *  - loader to use if not present
     */
    private abstract class Info {
      def name:String
      val ana:(Boolean,Boolean)=>Null//XXX Analyze
      //take advantage of the fact that isList/isSeq is a predefined constant in a given instance
      def loader(fd:FieldAnnot,cz:Class[_]) = if (cz!=classOf[Unknown]) cz.getName else {
        try {
          val l = getClass //XXX ana(fd.isList,fd.isSeq).getLoadableClass
          if (l.isInstanceOf[AnyRef]) l.getName else null
        } catch {
          case d:DynamicInvocation => ""
        }
      }
    }
    private object Info {
      //XXX val no = classOf[QName.NoProc]
      import scala.language.implicitConversions
      implicit protected final def canonicalName(m:Method) = { //XXX unused
        val n = m.getName
        if      (n.startsWith("set")) n.substring(3)
        else if (n.endsWith("_$eq"))  n.substring(0, n.length-5)
        else     n
      }
      implicit protected final def canonicalName(f:Field) = f.getName  //XXX unused
      implicit def apply(x:Annoted) = new Info {
        val name:String = x.getName
        val ana = null //XXX x match {
          //XXX case f:Field  => Analyze(f,null,_:Boolean,_:Boolean)
          //XXX case m:Method => Analyze(m,null,_:Boolean,_:Boolean)
        //XXX }
      }
    }
    
    /** qName Builder for a TagField
     */
    protected def qName(n:String, cz:Class[_<:QName.Builder]) = {
      if (cz==classOf[QName.NoProc])
        if      (n=="=")           QName.Const(null)
        else if (n.charAt(0)=='>') QName.Local(n.substring(1))
        else                       QName.Const(n)
      else
        cz.getConstructor(classOf[String]).newInstance(n);
    }
    /** Translate TagField into something usable.
     *  Rules for QName with the default (NoProc) processor:
     *  - outName = '>...'  => Local(...)   (note that outName='>!' means Local(inName)
     *  - outName = '='     => Const(null)  : use parsed name
     *  - outName other     => Const        : use outName
     */
    final private class TagFieldHelper(f:TagField,i:Info) extends FieldAnnot {
      val inName:String      = i.name
      val loader:String      = i.loader(this,f.loader())
      def qName              = tagManager.qName(f.outName(),f.qName());
      def min:Int            = f.min()
      def max:Int            = 1
      def check:String       = f.check()
      def valid:String       = f.valid()
      def param:String       = f.param()
      def convert:String     = f.convert()
      def audit:String       = f.audit()
      def contiguous:Boolean = false
      def isList:Boolean     = false
      def isSeq:Boolean      = false
      def isFld:Boolean      = loader==null
    }
    /** Translate TagSeq into something usable
     */
    final private class TagSeqHelper(f:TagSeq,i:Info) extends FieldAnnot {
      val inName:String      = i.name
      val loader:String      = i.loader(this,f.loader())
      def qName              = tagManager.qName(f.outName(),f.qName());
      def outName:String     = if (f.outName().isEmpty) inName else f.outName()
      def min:Int            = f.min()
      def max:Int            = f.max()
      def check:String       = f.check()
      def valid:String       = f.valid()
      def param:String       = f.param()
      def convert:String     = f.convert()
      def audit:String       = f.audit()
      def contiguous:Boolean = f.contiguous()
      def isList:Boolean     = false
      def isSeq:Boolean      = true
      def isFld:Boolean      = loader==null
   }
    /** Translate TagList into something usable
     */
    final private class TagListHelper(f:TagList,i:Info) extends FieldAnnot {
      val inName:String      = i.name
      val loader:String      = i.loader(this,f.loader())
      def qName              = tagManager.qName(f.outName(),f.qName());
      def min:Int            = f.min()
      def max:Int            = f.max()
      def check:String       = f.check()
      def valid:String       = f.valid()
      def param:String       = f.param()
      def convert:String     = f.convert()
      def audit:String       = f.audit()
      def contiguous:Boolean = false
      def isList:Boolean     = true
      def isSeq:Boolean      = false
      def isFld:Boolean      = loader==null
    }
    /** Translate TagStruct into something usable
     */
    final private class TagStructHelper(f:TagStruct, val clzz:Class[_<:AnyRef]) extends StructAnnot {
      def auditMin = f.auditMin()
      def auditMax = f.auditMax()
      def fast     = f.fast()
    }
    private def apply(a:Annotation,m:Annoted):FieldAnnot = a match {
      case x:TagField => new TagFieldHelper(x,m)
      case x:TagSeq   => new TagSeqHelper(x,m)
      case x:TagList  => new TagListHelper(x,m)
    }
    private def apply(a:Annotation,f:Field):FieldAnnot = a match {
      case x:TagField => new TagFieldHelper(x,f)
      case x:TagSeq   => new TagSeqHelper(x,f)
      case x:TagList  => new TagListHelper(x,f)
    }
  
    final private def defaultStructAnnotType(clzz0:Class[_<:AnyRef]) = new StructAnnot {
      def auditMin = 0
      def auditMax = 5
      def fast     = true  //reminder: engines that don't support this will set the used value to false on run time
      def clzz     = clzz0
    }
    final private val sortedFields = new HashMap[Class[_],Array[FieldAnnot]]
  
    /** recovers one and only one annotation in the given list */
    private final def which(x:Annoted, clzz:Class[_<:Annotation]*):FieldAnnot = {
      for (c <- clzz) {
        x.getAnnotation(c) match {
          case null       => 
          case t:TagField => return new TagFieldHelper(t,x)
          case t:TagSeq   => return new TagSeqHelper(t,x)
          case t:TagList  => return new TagListHelper(t,x)
        }
      }
      null
    }
    
    protected final def sort(x1:Annoted,x2:Annoted):Boolean = x1.getName.compareTo(x2.getName)<0
    protected final def notPublic(x:Annoted):Boolean        = !Modifier.isPublic(x.getModifiers)
    protected final def oneParam(m:Method):Boolean          = m.getParameterTypes.length==1
  
    /** Lists all fields/methods for a class, including superclass protected/private ones
     */
    protected final def getFields(cz:Class[_]):Array[FieldAnnot] = if (cz!=classOf[AnyRef]) {
      //find sorted fields for that class
      sortedFields.getOrElseUpdate(cz,
          ((cz.getFields                   ++ cz.getDeclaredFields.filter(notPublic)).sortWith(sort) ++
           (cz.getMethods.filter(oneParam) ++ cz.getDeclaredMethods.filter(notPublic)).sortWith(sort))
           .map(which(_,classOf[TagField],classOf[TagList],classOf[TagSeq])).filter(_!=null)
      )
    } else {
      new Array[FieldAnnot](0)
    }
    
    private final def annot(x:Class[_<:AnyRef]):StructAnnot = x.getAnnotation(classOf[TagStruct]) match {
      case null => defaultStructAnnotType(x)
      case a    => new TagStructHelper(a,x)
    }
  
    /** Here, the id is the full class name */
    final def apply(id:String):(StructAnnot,Traversable[FieldAnnot]) =
      if (id==null) null  //applies to non struct fields
      else {
        val cz=Class.forName(id).asSubclass(classOf[AnyRef])
        (annot(cz),getFields(cz))
      }
  }
}
