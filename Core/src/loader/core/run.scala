package loader.core
import loader.core.definition.Impl
import loader.core.definition.Processor

object run {
  
  /** The usual way to launch a pair parser/processor.
   *  The parameters look, well, ugly ; but the compiler will infer them nicely from standard functions.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Impl { type BaseParser>:P }]
        (p:P,m:M#Dlg)
        (
          u:p.UCtx[m.proc.type] with m.proc.UCtx[p.type],
          init: m.type => (m.proc.UCtx[p.type]) => p.Parser=>m.proc.Elt { type Builder=p.type },
          f:p.Parser { type Proc= m.proc.type } => Unit
        )
        :(p.Ret,m.proc.Ret)
     = p(u,init(m)(u)).invoke(f)
     
  /** creating an include, i.e. an element based on a different parser and a current top element. */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Impl { type BaseParser>:P }]
        (p:P,e:M#Elt)
        (
            u:p.UCtx[e.dlg.proc.type] with e.dlg.proc.UCtx[p.type],
            f:p.Parser { type Proc= e.dlg.proc.type } => Unit
        )
        :(p.Ret,e.dlg.proc.Ret)
     = include(e.dlg.proc)(p,e.myselfImpl)(u,f)
  
  /** the basic include call. Not really useful. */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }]
        (m:M)(p:P,e:m.Elt)
        (
            u:p.UCtx[m.type] with m.UCtx[p.type],
            f:p.Parser { type Proc= m.type } => Unit
        )
        :(p.Ret,m.Ret)
     = p[m.type](u,e.builder[p.type](_:p.Parser,u,e,e.status)).invoke(f)
  
  /** unsafe include, where types are coerced. There is no warranty that this will succeed.
   *  this will often be used, as includes are by nature unsafe.
   *  Likely exception: ClassCastException
   */
  def includeX[P<:ParserBuilder, M<:Impl]
        (p:P,e:M#EltBase)
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
    include(m1)(p1,e1)(u1,f1)  //nothing warrants that [P,M] satisfy the class constraints: the call may fail
  }
}