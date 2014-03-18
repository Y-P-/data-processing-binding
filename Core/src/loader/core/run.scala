package loader.core
import loader.core.definition.Impl
import loader.core.definition.Processor

object run {
  
  /** The usual way to launch a pair parser/processor.
   *  The parameters look, well, ugly ; but the compiler will infer them nicely from standard functions.
   *  Note that u is an acceptable user context type for both processor and parser.
   *  
   *  @param p, the parser
   *  @param m, the delegate (processor)
   *  @param u, the context
   *  @param init, a m._ method for creating the initializing function
   *  @param f, a p._ method for starting the parser
   *  @returns the return values of both the parser and the processor
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Impl { type BaseParser>:P }]
        (p:P,m:M#Dlg)
        (
          u:p.UCtx[m.proc.type] with m.proc.UCtx[p.type],
          init: m.type => (m.proc.UCtx[p.type] => p.Parser=>m.proc.Elt { type Builder=p.type }),
          f:p.Parser { type Proc= m.proc.type } => Unit
        )
        :(p.Ret,m.proc.Ret)
     = p(u,init(m)(u)).invoke(f)
     
  /** creating an include, i.e. an element based on a different parser and a current top element.
   *  @param p, the new parser
   *  @param e, the element on which the include is based
   *  @param u, the new context
   *  @param f, a p._ method for starting the parser
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Impl { type BaseParser>:P }]
        (p:P,e:M#Elt,reduce:Boolean)
        (
            u:p.UCtx[e.dlg.proc.type] with e.dlg.proc.UCtx[p.type],
            f:p.Parser { type Proc= e.dlg.proc.type } => Unit
        )
        :(p.Ret,e.dlg.proc.Ret)
     = include(e.dlg.proc)(p,e.myselfImpl,reduce)(u,f)
  
  /** the basic include call. Not extremely useful because it refers to m, which is implicit in e.
   *  @param m, the processor
   *  @param p, the new parser
   *  @param e, the element on which the include is based
   *  @param reduce, true if the 'include' level is to be erased (i.e. inclusion is done at the same level as the include element)
   *  @param u, the new context
   *  @param f, a p._ method for starting the parser
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }]
        (m:M)(p:P,e:m.Elt,reduce:Boolean)
        (
            u:p.UCtx[m.type] with m.UCtx[p.type],
            f:p.Parser { type Proc= m.type } => Unit
        )
        :(p.Ret,m.Ret)
     = p[m.type](u,(if (reduce) e.parent else e).copy[p.type](_:p.Parser,u)).invoke(f)
  
  /** unsafe include, where types are coerced. There is no warranty that this will succeed.
   *  this will often be used, as includes are by nature unsafe.
   *  @param m, the processor
   *  @param p, the new parser
   *  @param e, the element on which the include is based
   *  @param reduce, true if the 'include' level is to be erased (i.e. inclusion is done at the same level as the include element)
   *  @param u, the new context
   *  @param f, a p._ method for starting the parser
   *  @throws ClassCastException if p or u are not acceptable by e
   */
  def includeX[P<:ParserBuilder, M<:Impl]
        (p:P,e:M#EltBase,reduce:Boolean)
        (
            u:UsrCtx[P,M],                                     //can meet with disaster
            f:p.Parser { type Proc= e.dlg.proc.type } => Unit  //will not fail
        )
        :(p.Ret,e.dlg.proc.Ret)
     = {
    //note that all the following casts will always succeed.
    //on the JVM, erasure will ensure that everything is OK.
    //however, the include call may itself fail with wild exceptions
    type P1 = ParserBuilder { type BaseProcessor=definition.Impl; type Ret=p.Ret }
    type M1 = Impl { type BaseParser = ParserBuilder; type Ret=e.dlg.proc.Ret }
    val p1                                          = p.asInstanceOf[P1]
    val m1                                          = e.dlg.proc.asInstanceOf[M1]
    val e1:m1.Elt                                   = e.asInstanceOf[m1.Elt]
    val u1:p1.UCtx[m1.type] with m1.UCtx[p1.type]   = u.asInstanceOf[p1.UCtx[m1.type] with m1.UCtx[p1.type]]
    val f1:p1.Parser { type Proc= m1.type } => Unit = f.asInstanceOf[p1.Parser { type Proc= m1.type } => Unit]
    if (!p1.baseProcessorClass.isAssignableFrom(m1.getClass))
      throw new ClassCastException(s"${m1.getClass} is not an acceptable processor class for parser ${p1.getClass}: required _ <: ${p1.baseProcessorClass}")
    if (!m1.baseParserClass.isAssignableFrom(p1.getClass))
      throw new ClassCastException(s"${p1.getClass} is not an acceptable parser class for processor ${m1.getClass}: required _ <: ${m1.baseParserClass}")
    if (!p1.baseUCtxClass.isAssignableFrom(u1.getClass))
      throw new ClassCastException(s"${u1.getClass} is not an acceptable user context class for parser ${p1.getClass}: required _ <: ${p1.baseUCtxClass}")
    if (!m1.baseUCtxClass.isAssignableFrom(u1.getClass))
      throw new ClassCastException(s"${u1.getClass} is not an acceptable user context class for processor ${m1.getClass}: required _ <: ${m1.baseUCtxClass}")
    include(m1)(p1,e1,reduce)(u1,f1)  //nothing warrants that [P,M] satisfy the class constraints: the call may fail
  }
}