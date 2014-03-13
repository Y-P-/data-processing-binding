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
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }]
        (m:M)(e:m.Elt,p:P)
        (
            u:p.UCtx[m.type] with m.UCtx[p.type],
            f:p.Parser { type Proc= m.type } => Unit
        )
        :(p.Ret,m.Ret)
     = p[m.type](u,e.builder[p.type](_:p.Parser,u,e,null)).invoke(f)
}