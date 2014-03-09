package loader.core

import scala.collection.mutable.HashMap
import context.Context

/** A possible partial implementation for Def.
 *  We are missing the onBeg/onVal/onEnd/onChild/getData implementations, which we be provided by the actual processing engine.
 *  Despite its relative inefficiency (because it is very generic and redirects all calls from Elt to Motor), we will want
 *  to use it as much as possible, if only not to redeclare the nine inner classes and the not so simple builder...
 *  A well written motor can often be used indifferently with the Ext/Ctx implementation with little fuss (not always though:
 *  see Struct for a exemple that works, and Obj for one that doesn't.)
 *  
 *  Of course, if we want more efficiency, we can:
 *  o decide not to derive ElementBase from proc
 *  o dispatch and code the processor logic directly within ElementBase/Struct/List/Terminal
 *  o define the additional classes for callback handling mostly as below
 *  o write the builder mostly as below
 *  This would only be usefull in a handful of cases, after it is proved that this generic processor is the cause of inefficiency...
 */
trait CtxCore extends definition.Impl {
  type Status = CtxCore.Status[Key]
  protected[this] def noStatus(fd:Context#FieldMapping) = new Status(noKey,1,fd,false)

  protected[this] type Data
  def getData[P<:BaseParser](parent:Element[P],s:Status):Data
    
  trait Elt[-P<:BaseParser] extends super.Elt[P] { this:Elt[P] with Element[P]=>
    def idx: Int
    def fd: Context#FieldMapping
    /** gets the previous Struct layer */
    def parentStc:Struct[P] = parent match {
      case s:Struct[P]  => s
      case e:Elt[P]     => e.parentStc
      case _         => null
    }
    //the 'official' list of names that uniquely identify that loader ; seq numbers or list index are used where necessary.
    def nameSeq:Traversable[String] = for (e<-this) yield e.rankedName
   
    //the name of this element, accounting for the fact that is can be anonymous, in which case its name becomes its rank
    def rankedName = if (fd.isSeq) s"${if(!name.isEmpty) s"$name." else ""}${idx.toString}" else name
    override def print(out:java.io.Writer):Unit = foreach(e=>out.write(s".${e.rankedName}"))
  }

  //parent is undefined here
  trait Struct[-P<:BaseParser] extends Elt[P] { this:Element[P]=>
    /** the known tags for that struct */
    val tags = fd.loader.fields
    /** tags seen count */
    val seen = HashMap.empty[String, Int]
    /** the last fd seen in that struct */
    private var previous: Context#FieldMapping = null
    /** Tells whether that Struct can ask for fast forward to the parser */
    val canFast = userCtx.fast && fd.loader.annot.fast
    val doFast  = canFast && !tags.hasNonContig
  
    override protected def onName(key: Key):Status = {
      val name = key.toString
      tags.fetch(name) match {
        case None     =>
          if (seen.size == tags.size && doFast) throw ParserBuilder.skipEnd
          else                                  throw ParserBuilder.skip
        case Some(fd) =>
          val fd1 = if (fd.annot.loader==null || fd.annot.loader.length>0) fd else null //XXX userCtx.solveDynamic(this,fd)
          val idx = seen.getOrElse(name, 0) + 1
          seen.update(name, idx)
          var broken: Boolean = idx != 1 //for non seq, idx!=1 indicates a multiple occurence ; for a seq, the possibility that the seq is broken
          if (fd1.isSeq) broken &&= fd1.isSeq && previous != null && previous != fd1 //check last fd to see if the current seq has been broken
          previous = fd1
          //XXX manage broken data (keep it ? discard it ?)
          new Status(key, idx, fd1, broken)
      }
    }
  }
  //parent is undefined here
  trait List[-P<:BaseParser] extends Elt[P] { this:Element[P]=>
    private val innerFd: Context#FieldMapping = fd.asSeq
    var index: Int = 0
    override protected def onName(key: Key):Status = {
      val name = key.toString
      if (!name.isEmpty) throw new IllegalStateException(s"illegal field $name in a list")
      index += 1
      new Status(key, index, innerFd, false)
    }
  }
  //parent is undefined here
  trait Terminal[-P<:BaseParser] extends Elt[P] { this:Element[P]=>
    override protected def onName(key: Key): Status = throw new IllegalStateException(s"illegal field $key in the terminal field $name")
    override protected def onEnd():Ret              = throw new IllegalStateException(s"cannot pull a simple field : '$name'")
  }
  
  type Motor[-P<:BaseParser] = Launcher[P]
  trait Launcher[-P<:BaseParser] extends super.Launcher[P] {
    //a default stub ; it has to be overriden by the Struct/List/Terminal implementation
    final def onName(e:Element[_<:P],key:Key): Nothing  = ???
    //top factories
    def apply(fd:Context#FieldMapping,cbks:Cbks[P]*):Parser[P]=>Element[P] = apply(noStatus(fd), cbks:_*)
    def apply(fd:Context#FieldMapping):Parser[P]=>Element[P]               = apply(noStatus(fd))
  }
  
  def builder[P<:BaseParser](m:Motor[P]) = new Bld[P] {
    def apply(parser:Parser[P], parent: Element[P], s: Status) =
      if      (s.fd.isList)   new XList(parser, m, s, parent)
      else if (s.fd.isStruct) new XStruct(parser, m, s, parent)
      else                    new XTerminal(parser, m, s, parent)
    def apply(parser:Parser[P], parent: Element[P], s: Status, cbks: Cbks[P]*) =
      if      (s.fd.isList)   new XListCbks(parser, m, s, parent, cbks:_*)
      else if (s.fd.isStruct) new XStructCbks(parser, m, s, parent, cbks:_*)
      else                    new XTerminalCbks(parser, m, s, parent, cbks:_*)
    def apply(parser:Parser[P], parent: Element[P], s: Status, cb:Cbk[P], cbks: Cbks[P]*) =
      if      (s.fd.isList)   new XListCbk(parser, m, s, parent, cb, cbks:_*)
      else if (s.fd.isStruct) new XStructCbk(parser, m, s, parent, cb, cbks:_*)
      else                    new XTerminalCbk(parser, m, s, parent, cb, cbks:_*)
  }
  
  //concrete class definitions
  abstract class Element[-P<:BaseParser](parser:Parser[P], motor:Motor[P], key:Key, parent:Element[P], val fd:Context#FieldMapping, val idx:Int, val data:Data) extends super.EltBase[P](parser,motor,key,parent) with Elt[P] {
    def this(parser:Parser[P], motor:Motor[P], s:Status, parent:Element[P]) = this(parser,motor,s.key,parent,s.fd,s.idx,getData(parent, s))
  }
  protected[this] class XStruct[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P])                             extends Element(parser,motor,s,parent) with Struct[P]
  protected[this] class XList[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P])                               extends Element(parser,motor,s,parent) with List[P]
  protected[this] class XTerminal[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P])                           extends Element(parser,motor,s,parent) with Terminal[P]
  protected[this] class XStructCbks[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P],val cbks:Cbks[P]*)          extends XStruct(parser,motor,s,parent) with WithCallbacks[P]
  protected[this] class XListCbks[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P],val cbks:Cbks[P]*)            extends XList(parser,motor,s,parent) with WithCallbacks[P]
  protected[this] class XTerminalCbks[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P],val cbks:Cbks[P]*)        extends XTerminal(parser,motor,s,parent) with WithCallbacks[P]
  protected[this] class XStructCbk[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P],val cb:Cbk[P],cbks:Cbks[P]*)    extends XStructCbks(parser,motor,s,parent,cbks:_*) with WithCallback[P]
  protected[this] class XListCbk[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status,parent:Element[P],val cb:Cbk[P],cbks:Cbks[P]*)      extends XListCbks(parser,motor,s,parent,cbks:_*) with WithCallback[P]
  protected[this] class XTerminalCbk[P<:BaseParser](parser:Parser[P],motor:Motor[P],s:Status, parent:Element[P],val cb:Cbk[P],cbks:Cbks[P]*) extends XTerminalCbks(parser,motor,s,parent,cbks:_*) with WithCallback[P]     
}
object CtxCore {
  class Status[K>:Null](key:K, val idx: Int, val fd: Context#FieldMapping, val broken: Boolean) extends ExtCore.Status(key)
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract[+D] extends CtxCore { protected[this] type Data=D }
}
