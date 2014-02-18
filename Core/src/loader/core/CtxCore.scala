package loader.core

import scala.collection.mutable.HashMap
import context.Context

object CtxCore {
  
  class Status(name:String, val idx: Int, val fd: Context#FieldMapping, val broken: Boolean) extends Core.Status(name)

  /** A quite complex implementation for core, where Element is broken down in three possible states:
   *  - Struct   (containing any kind of sub-Element),
   *  - List     (containing sequences of identical sub-Elements),
   *  - Terminal (receiving terminal String and having no children)
   *  This also assumes that any Element is somehow declared (Context#FieldMapping) so that we know
   *  at any time what to expect by simply watching the name.
   */
  trait Def extends ExtCore.Def {
    override type Status = CtxCore.Status
    type Element >: Null <: Elt

    sealed trait Elt extends super.Elt { this:Elt with Element=>
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
    trait Struct extends Elt { this:Element=>
      /** the known tags for that struct */
      val tags = fd.loader.fields
      /** tags seen count */
      val seen = HashMap.empty[String, Int]
      /** the last fd seen in that struct */
      private var previous: Context#FieldMapping = null
      /** Tells whether that Struct can ask for fast forward to the parser */
      val canFast = userCtx.fast && fd.loader.annot.fast
      val doFast  = canFast && !tags.hasNonContig
    
      override protected def onName(name: String) = tags.fetch(name) match {
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
          new CtxCore.Status(name, idx, fd1, broken) //XXX see pushAttr to set this boolean to true
      }
    }
    //parent is undefined here
    trait List extends Elt { this:Element=>
      private val innerFd: Context#FieldMapping = fd.asSeq
      var index: Int = 0
      override protected def onName(name: String):CtxCore.Status = {
        if (!name.isEmpty) throw new IllegalStateException(s"illegal field $name in a list")
        index += 1
        new CtxCore.Status(name, index, innerFd, false)
      }
    }
    //parent is undefined here
    trait Terminal extends Elt { this:Element=>
      override protected def onName(name: String): Status = throw new IllegalStateException(s"illegal field $name in a terminal field")
      override protected def onEnd():Ret                  = throw new IllegalStateException(s"cannot pull a simple field : '$name'")
    }
  }

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
  trait Impl extends definition.Impl with Def {
    type Element = Elt
    
    abstract class Launcher[-BP<:BaseParser with Singleton](bp:BP) extends super.Launcher[BP](bp) { impl=>
      protected def getData(parent:Element,s:Status):Data
      
      //implementations : top builders
      def apply(fd:Context#FieldMapping,cbks:Cbks*) = attach(new Status("",1,fd,false), cbks:_*)
      def apply(fd:Context#FieldMapping)            = attach(new Status("",1,fd,false))
      
      //a default stub ; it will be overriden by the Struct/List/Terminal implementation
      protected def onName(e:Element,name:String): Status  = null
      
      //concrete definitions
      class ElementBase(protected var parser0:Parser, val name:String, val parent:Element, val fd:Context#FieldMapping, val idx:Int, val childBuilder:Bld, val data:Data) extends Processor with Elt {
        def this(parser:Parser, s:Status, parent:Element, childBuilder:Bld) = this(parser,s.name,parent,s.fd,s.idx,childBuilder,impl.getData(parent, s))
        def getData(s: Status) = Launcher.this.getData(this,s)
      }
      class Struct(parser:Parser,s:Status,parent:Element,childBuilder:Bld)                             extends ElementBase(parser,s,parent,childBuilder) with Impl.this.Struct
      class List(parser:Parser,s:Status,parent:Element,childBuilder:Bld)                               extends ElementBase(parser,s,parent,childBuilder) with Impl.this.List
      class Terminal(parser:Parser,s:Status,parent:Element,childBuilder:Bld)                           extends ElementBase(parser,s,parent,childBuilder) with Impl.this.Terminal
      class StructCbks(parser:Parser,s:Status,parent:Element,childBuilder:Bld,val cbks:Cbks*)          extends Struct(parser,s,parent,childBuilder) with WithCallbacks
      class ListCbks(parser:Parser,s:Status,parent:Element,childBuilder:Bld,val cbks:Cbks*)            extends List(parser,s,parent,childBuilder) with WithCallbacks
      class TerminalCbks(parser:Parser,s:Status,parent:Element,childBuilder:Bld,val cbks:Cbks*)        extends Terminal(parser,s,parent,childBuilder) with WithCallbacks
      class StructCbk(parser:Parser,s:Status,parent:Element,childBuilder:Bld,val cb:Cbk,cbks:Cbks*)    extends StructCbks(parser,s,parent,childBuilder,cbks:_*) with WithCallback
      class ListCbk(parser:Parser,s:Status,parent:Element,childBuilder:Bld,val cb:Cbk,cbks:Cbks*)      extends ListCbks(parser,s,parent,childBuilder,cbks:_*) with WithCallback
      class TerminalCbk(parser:Parser,s:Status, parent:Element,childBuilder:Bld,val cb:Cbk,cbks:Cbks*) extends TerminalCbks(parser,s,parent,childBuilder,cbks:_*) with WithCallback
      
      override val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, s: Status, childBuilder:Bld) =
          if      (s.fd.isList)   new List(parser, s, parent, childBuilder)
          else if (s.fd.isStruct) new Struct(parser, s, parent, childBuilder)
          else                    new Terminal(parser, s, parent, childBuilder)
        def apply(parser:Parser, parent: Element, s: Status, childBuilder:Bld, cbks: Cbks*) =
          if      (s.fd.isList)   new ListCbks(parser, s, parent, childBuilder, cbks:_*)
          else if (s.fd.isStruct) new StructCbks(parser, s, parent, childBuilder, cbks:_*)
          else                    new TerminalCbks(parser, s, parent, childBuilder, cbks:_*)
        def apply(parser:Parser, parent: Element, s: Status, childBuilder:Bld, cb:Cbk, cbks: Cbks*) =
          if      (s.fd.isList)   new ListCbk(parser, s, parent, childBuilder, cb, cbks:_*)
          else if (s.fd.isStruct) new StructCbk(parser, s, parent, childBuilder, cb, cbks:_*)
          else                    new TerminalCbk(parser, s, parent, childBuilder, cb, cbks:_*)
      }
      
      //XXX for fun...
      def copy[T<:ElementBase:scala.reflect.ClassTag](b:T,p1: String):T = {
        import scala.reflect.runtime.{ currentMirror => cm }
        import scala.reflect.runtime.universe._
        val im = cm.reflect(b)
        val ts = im.symbol.typeSignature
        val copySym = ts.member(newTermName("copy")).asMethod
        def element(p: Symbol): Any = (im reflectMethod ts.member(p.name).asMethod)()
        val args = for (ps <- copySym.paramss; p <- ps) yield {
          if (p.name.toString == "p1") p1 else element(p)
        }
        (im reflectMethod copySym)(args: _*).asInstanceOf[T]
      }
    }
  }
}