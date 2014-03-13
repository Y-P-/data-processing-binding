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
  type Dlg<:DlgBase
  type Elt = EltBase
  protected[this] def noStatus(fd:Context#FieldMapping) = new Status(noKey,1,fd,false)

  protected[this] type Data>:Null
  def getData(parent:Elt,s:Status):Data
    
  trait EltBase extends super.EltBase {
    def data: Data
    def idx: Int
    def fd: Context#FieldMapping
    /** gets the previous Struct layer */
    def parentStc:Struct = parent match {
      case s:Struct  => s
      case e:Elt     => e.parentStc
      case _         => null
    }
    //the 'official' list of names that uniquely identify that loader ; seq numbers or list index are used where necessary.
    def nameSeq:Traversable[String] = for (e<-this) yield e.rankedName
   
    //the name of this element, accounting for the fact that is can be anonymous, in which case its name becomes its rank
    def rankedName = if (fd.isSeq) s"${if(!name.isEmpty) s"$name." else ""}${idx.toString}" else name
    override def print(out:java.io.Writer):Unit = foreach(e=>out.write(s".${e.rankedName}"))
  }

  //parent is undefined here
  trait Struct extends EltBase {
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
  trait List extends EltBase {
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
  trait Terminal extends EltBase {
    override protected def onName(key: Key): Status = throw new IllegalStateException(s"illegal field $key in the terminal field $name")
    override protected def onEnd():Ret              = throw new IllegalStateException(s"cannot pull a simple field : '$name'")
  }
  
  trait DlgBase extends super.DlgBase { this:Dlg=>
    //a default stub ; it has to be overriden by the Struct/List/Terminal implementation
    final def onName(e:Elt,key:Key): Nothing  = ???
  }
  
  def builder(dlg:Dlg) = new EltBuilder {
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, s: Status) =
      if      (s.fd.isList)   new XList(parser, userCtx, dlg, s, parent)
      else if (s.fd.isStruct) new XStruct(parser, userCtx, dlg, s, parent)
      else                    new XTerminal(parser, userCtx, dlg, s, parent)
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, s: Status, cbks: Cbks*) =
      if      (s.fd.isList)   new XListCbks(parser, userCtx, dlg, s, parent, cbks:_*)
      else if (s.fd.isStruct) new XStructCbks(parser, userCtx, dlg, s, parent, cbks:_*)
      else                    new XTerminalCbks(parser, userCtx, dlg, s, parent, cbks:_*)
    def apply[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], parent: Elt, s: Status, cb:Cbk, cbks: Cbks*) =
      if      (s.fd.isList)   new XListCbk(parser, userCtx, dlg, s, parent, cb, cbks:_*)
      else if (s.fd.isStruct) new XStructCbk(parser, userCtx, dlg, s, parent, cb, cbks:_*)
      else                    new XTerminalCbk(parser, userCtx, dlg, s, parent, cb, cbks:_*)
  }

  def apply[X<:BaseParser with Singleton](u:UCtx[X],fd:Context#FieldMapping,dlg:Dlg)           :X#Parser=>Element[X] = dlg.builder(_,u,null,noStatus(fd))
  def apply[X<:BaseParser with Singleton](u:UCtx[X],fd:Context#FieldMapping,dlg:Dlg,cbks:Cbks*):X#Parser=>Element[X] = dlg.builder(_,u,null,noStatus(fd),cbks:_*)
  
  //concrete class definitions
  protected abstract class Element[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, key:Key, parent:Elt, val fd:Context#FieldMapping, val idx:Int, val data:Data) extends ElementBase[X](parser,userCtx,dlg,key,parent) with Elt {
    def this(parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, s:Status, parent:Elt) = this(parser,userCtx,dlg,s.key,parent,s.fd,s.idx,getData(parent, s))
  }
  protected class XStruct      [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt)                       extends Element(parser,userCtx,dlg,s,parent) with Struct
  protected class XList        [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt)                       extends Element(parser,userCtx,dlg,s,parent) with List
  protected class XTerminal    [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt)                       extends Element(parser,userCtx,dlg,s,parent) with Terminal
  protected class XStructCbks  [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cbks:Cbks*)        extends XStruct(parser,userCtx,dlg,s,parent) with WithCallbacks
  protected class XListCbks    [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cbks:Cbks*)        extends XList(parser,userCtx,dlg,s,parent) with WithCallbacks
  protected class XTerminalCbks[X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cbks:Cbks*)        extends XTerminal(parser,userCtx,dlg,s,parent) with WithCallbacks
  protected class XStructCbk   [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cb:Cbk,cbks:Cbks*) extends XStructCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback
  protected class XListCbk     [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cb:Cbk,cbks:Cbks*) extends XListCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback
  protected class XTerminalCbk [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cb:Cbk,cbks:Cbks*) extends XTerminalCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback        
}
object CtxCore {
  class Status[K>:Null](key:K, val idx: Int, val fd: Context#FieldMapping, val broken: Boolean) extends ExtCore.Status(key)
  //using Abstract prevents code bloating due to trait expension
  abstract class Abstract[+D>:Null] extends CtxCore { protected[this] type Data=D }
}
