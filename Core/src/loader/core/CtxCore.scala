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
  type Dlg>:Null<:DlgBase
  type Elt = EltBase
  protected[this] def noStatus(fd:Context#FieldMapping) = new Status(noKey,1,fd,false)

  protected[this] type Data>:Null
    
  trait EltBase extends super.EltBase {
    def data: Data
    def idx: Int
    def fd: Context#FieldMapping
    def kind:Int
    /** gets the previous Struct layer */
    def parentStc:Struct = parent match {
      case s:Struct  => s
      case e:Elt     => e.parentStc
      case _         => null
    }
    //the 'official' list of names that uniquely identify that loader ; seq numbers or list index are used where necessary.
    def nameSeq:Traversable[String] = for (e<-this) yield e.rankedName
    def status = new Status(key,idx,fd,false)
   
    //the name of this element, accounting for the fact that is can be anonymous, in which case its name becomes its rank
    def rankedName = if (fd.isSeq) s"${if(!name.isEmpty) s"$name." else ""}${idx.toString}" else name
    override def print(out:java.io.Writer):Unit = foreach(e=>out.write(s".${e.rankedName}"))
    
    /** This trait is used to create copies of the current Elt for changing parser and userCtx.
     *  It must refer to the same fields as the copied item.
     *  It must be "transparent", i.e. must not make any new method call (otehrwise methods with side effects would pose problems.)
     */
    protected[this] trait Copy {this:EltBase=>
      override val data = EltBase.this.data //refer to the already computed data
      def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]) = EltBase.this.copy(p,u)  //idempotent
    }    
  }
  
  //parent is undefined here
  trait Struct extends EltBase {
    /** the known tags for that struct */
    val tags = fd.loader.fields
    /** tags seen count */
    val seen = HashMap.empty[String, Int]
    /** the last fd seen in that struct */
    protected[this] var previous:Context#FieldMapping
    /** Tells whether that Struct can ask for fast forward to the parser */
    val canFast = eltCtx.fast && fd.loader.annot.fast
    val doFast  = canFast && !tags.hasNonContig
    final def kind=CtxCore.struct
    
    protected[this] trait Copy extends super.Copy { this:Struct=>
      override val tags = Struct.this.tags 
      override val seen = Struct.this.seen 
      override val canFast = Struct.this.canFast
      override val doFast = Struct.this.doFast
      override protected def previous=Struct.this.previous
      override protected def previous_=(fd:Context#FieldMapping) = Struct.this.previous=fd
    }
  
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
  trait List extends EltBase {
    final def kind=CtxCore.list
    protected val innerFd: Context#FieldMapping = fd.asSeq
    protected[this] var index0: Int
    def index = index0
    protected[this] trait Copy extends super.Copy { this:List=>
      override protected val innerFd=List.this.innerFd
      override protected[this] def index0:Int = List.this.index0
      override protected[this] def index0_=(i:Int):Unit = List.this.index0=i
    }
    override protected def onName(key: Key):Status = {
      val name = key.toString
      if (!name.isEmpty) throw new IllegalStateException(s"illegal field $name in a list")
      index0 += 1
      new Status(key, index, innerFd, false)
    }
  }
  trait Terminal extends EltBase {
    final def kind=CtxCore.terminal
    override protected def onName(key: Key): Status = throw new IllegalStateException(s"illegal field $key in the terminal field $name")
    override protected def onEnd():Ret              = throw new IllegalStateException(s"cannot pull a simple field : '$name'")
  }
  
  trait DlgBase extends super.DlgBase { dlg:Dlg=>
    def getData(parent:Elt):Data
    final def onName(e:Elt,key:Key): Nothing  = ???  //a default stub ; it is overriden by the Struct/List/Terminal implementation
    def apply[X<:BaseParser with Singleton](u:UCtx[X],fd:Context#FieldMapping): X#Parser=>Element[X]  = builder(_,u,null,noStatus(fd))
    def apply[X<:BaseParser with Singleton](u:UCtx[X],fd:Context#FieldMapping,cbks:Cbks*): X#Parser=>Element[X]  = builder(_,u,null,noStatus(fd),cbks:_*)
    def apply[X<:BaseParser with Singleton](fd:Context#FieldMapping): UCtx[X] => X#Parser=>Element[X] = apply(_,fd)
    def apply[X<:BaseParser with Singleton](fd:Context#FieldMapping,cbks:Cbks*): UCtx[X] => X#Parser=>Element[X] = apply(_,fd,cbks:_*)
  
    val builder = new EltBuilder {
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
  }

  def apply[X<:BaseParser with Singleton](fd:Context#FieldMapping): (UCtx[X],Dlg,Cbks*) => X#Parser=>Element[X] = (u,dlg,cbks) => dlg.builder(_,u,null,noStatus(fd),cbks:_*)
  
  //concrete class definitions
  protected abstract class Element[X<:BaseParser with Singleton](parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, key:Key, parent:Elt, val fd:Context#FieldMapping, val idx:Int) extends ElementBase[X](parser,userCtx,dlg,key,parent) with Elt {
    val data = dlg.getData(this)
    def this(parser:X#Parser, userCtx:UCtx[X], dlg:Dlg, s:Status, parent:Elt) = this(parser,userCtx,dlg,s.key,parent,s.fd,s.idx)
  }
  protected class XStruct[X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt) extends Element(parser,userCtx,dlg,s,parent) with Struct {
    protected var previous0:Context#FieldMapping=null
    protected def previous:Context#FieldMapping = previous0
    protected def previous_=(fd:Context#FieldMapping) = previous0=fd
    protected class Copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P],val cb:Cbk,val cbks:Cbks*) extends Element[P](p,u,dlg,key,parent,fd,idx) with Struct with super.Copy
    def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,null,null)
  }
  protected class XList[X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt) extends Element(parser,userCtx,dlg,s,parent) with List {
    protected[this] var index0:Int = 0
    protected class Copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P],val cb:Cbk,val cbks:Cbks*) extends Element[P](p,u,dlg,key,parent,fd,idx) with List with super.Copy
    def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,null,null)
  }
  protected class XTerminal[X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt) extends Element(parser,userCtx,dlg,s,parent) with Terminal {
    protected class Copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P],val cb:Cbk,val cbks:Cbks*) extends Element[P](p,u,dlg,key,parent,fd,idx) with Terminal with super.Copy
    def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,null,null)
  }
  protected class XStructCbks  [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cbks:Cbks*) extends XStruct(parser,userCtx,dlg,s,parent) with WithCallbacks {
    override def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,null,cbks:_*) with WithCallbacks
  }
  protected class XListCbks    [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cbks:Cbks*) extends XList(parser,userCtx,dlg,s,parent) with WithCallbacks {
    override def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,null,cbks:_*) with WithCallbacks
  }
  protected class XTerminalCbks[X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cbks:Cbks*) extends XTerminal(parser,userCtx,dlg,s,parent) with WithCallbacks {
    override def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,null,cbks:_*) with WithCallbacks
  }
  protected class XStructCbk   [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cb:Cbk,cbks:Cbks*) extends XStructCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback {
    override def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,cb,cbks:_*) with WithCallback
  }
  protected class XListCbk     [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cb:Cbk,cbks:Cbks*) extends XListCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback {
    override def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,cb,cbks:_*) with WithCallback
  }
  protected class XTerminalCbk [X<:BaseParser with Singleton](parser:X#Parser,userCtx:UCtx[X],dlg:Dlg,s:Status,parent:Elt,val cb:Cbk,cbks:Cbks*) extends XTerminalCbks(parser,userCtx,dlg,s,parent,cbks:_*) with WithCallback {
    override def copy[P<:BaseParser with Singleton](p:P#Parser,u:UCtx[P]):Elt { type Builder=P } = new Copy(p,u,cb,cbks:_*) with WithCallback         
  }
}
object CtxCore {
  class Status[K>:Null](key:K, val idx: Int, val fd: Context#FieldMapping, val broken: Boolean) extends ExtCore.Status(key)

  val struct=0
  val list=1
  val terminal=2
  
  /** Using Abstract prevents code bloating due to trait expansion
   *  You need to implement:
   *  - val noKey:Key
   *  - class Dlg extends DlgBase
   *  - def getData(p:Elt,s:Status)
   *  - appropriate constructors/factories for Dlg (no specific pattern here)
   */
  abstract class Abstract[+D>:Null] extends CtxCore { protected[this] type Data=D }
}
