package loader.core

import scala.collection.mutable.HashMap
import context.Context

object CtxCore {
  
  class Status[+K>:Null](key:K, val idx: Int, val fd: Context#FieldMapping, val broken: Boolean) extends Core.Status(key)

  /** A quite complex implementation for core, where Element is broken down in three possible states:
   *  - Struct   (containing any kind of sub-Element),
   *  - List     (containing sequences of identical sub-Elements),
   *  - Terminal (receiving terminal String and having no children)
   *  This also assumes that any Element is somehow declared (Context#FieldMapping) so that we know
   *  at any time what to expect by simply watching the name.
   */
  trait Processor extends ExtCore.Processor {
    override type Status = CtxCore.Status[Key]
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
    
      override protected def onName(key: Key):CtxCore.Status[Key] = {
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
            new CtxCore.Status(key, idx, fd1, broken) //XXX see pushAttr to set this boolean to true
        }
      }
    }
    //parent is undefined here
    trait List extends Elt { this:Element=>
      private val innerFd: Context#FieldMapping = fd.asSeq
      var index: Int = 0
      override protected def onName(key: Key):CtxCore.Status[Key] = {
        val name = key.toString
        if (!name.isEmpty) throw new IllegalStateException(s"illegal field $name in a list")
        index += 1
        new CtxCore.Status(key, index, innerFd, false)
      }
    }
    //parent is undefined here
    trait Terminal extends Elt { this:Element=>
      override protected def onName(key: Key): Status = throw new IllegalStateException(s"illegal field $key in the terminal field $name")
      override protected def onEnd():Ret              = throw new IllegalStateException(s"cannot pull a simple field : '$name'")
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
  trait Impl extends definition.Impl with Processor {
    type Element = Elt
    
    abstract class Launcher extends super.Launcher { impl=>
      protected def getData(parent:Element,s:Status):Data
      protected[this] def noStatus(fd:Context#FieldMapping) = new Status(noKey,1,fd,false)
      
      //implementations : top builders
      def apply(fd:Context#FieldMapping,cbks:Cbks*):Parser=>Element = apply(noStatus(fd), cbks:_*)
      def apply(fd:Context#FieldMapping):Parser=>Element            = apply(noStatus(fd))
      
      //a default stub ; it will be overriden by the Struct/List/Terminal implementation
      protected def onName(e:Element,key:Key): Status  = null
      
      //concrete definitions
      class ElementBase(protected var parser0:Parser, val key:Key, val parent:Element, val fd:Context#FieldMapping, val idx:Int, val builder:Bld, val data:Data) extends Inner with Elt {
        def this(parser:Parser, s:Status, parent:Element, builder:Bld) = this(parser,s.key,parent,s.fd,s.idx,builder,impl.getData(parent, s))
        def getData(s: Status) = Launcher.this.getData(this,s)
      }
      class Struct(parser:Parser,s:Status,parent:Element,builder:Bld)                             extends ElementBase(parser,s,parent,builder) with Impl.this.Struct
      class List(parser:Parser,s:Status,parent:Element,builder:Bld)                               extends ElementBase(parser,s,parent,builder) with Impl.this.List
      class Terminal(parser:Parser,s:Status,parent:Element,builder:Bld)                           extends ElementBase(parser,s,parent,builder) with Impl.this.Terminal
      class StructCbks(parser:Parser,s:Status,parent:Element,builder:Bld,val cbks:Cbks*)          extends Struct(parser,s,parent,builder) with WithCallbacks
      class ListCbks(parser:Parser,s:Status,parent:Element,builder:Bld,val cbks:Cbks*)            extends List(parser,s,parent,builder) with WithCallbacks
      class TerminalCbks(parser:Parser,s:Status,parent:Element,builder:Bld,val cbks:Cbks*)        extends Terminal(parser,s,parent,builder) with WithCallbacks
      class StructCbk(parser:Parser,s:Status,parent:Element,builder:Bld,val cb:Cbk,cbks:Cbks*)    extends StructCbks(parser,s,parent,builder,cbks:_*) with WithCallback
      class ListCbk(parser:Parser,s:Status,parent:Element,builder:Bld,val cb:Cbk,cbks:Cbks*)      extends ListCbks(parser,s,parent,builder,cbks:_*) with WithCallback
      class TerminalCbk(parser:Parser,s:Status, parent:Element,builder:Bld,val cb:Cbk,cbks:Cbks*) extends TerminalCbks(parser,s,parent,builder,cbks:_*) with WithCallback
      
      override val builder:Bld = new Bld {
        def apply(parser:Parser, parent: Element, s: Status, builder:Bld) =
          if      (s.fd.isList)   new List(parser, s, parent, builder)
          else if (s.fd.isStruct) new Struct(parser, s, parent, builder)
          else                    new Terminal(parser, s, parent, builder)
        def apply(parser:Parser, parent: Element, s: Status, builder:Bld, cbks: Cbks*) =
          if      (s.fd.isList)   new ListCbks(parser, s, parent, builder, cbks:_*)
          else if (s.fd.isStruct) new StructCbks(parser, s, parent, builder, cbks:_*)
          else                    new TerminalCbks(parser, s, parent, builder, cbks:_*)
        def apply(parser:Parser, parent: Element, s: Status, builder:Bld, cb:Cbk, cbks: Cbks*) =
          if      (s.fd.isList)   new ListCbk(parser, s, parent, builder, cb, cbks:_*)
          else if (s.fd.isStruct) new StructCbk(parser, s, parent, builder, cb, cbks:_*)
          else                    new TerminalCbk(parser, s, parent, builder, cb, cbks:_*)
      }
    }
  }
}