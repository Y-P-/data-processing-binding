package loader.core
import loader.core.definition.Impl

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


/*
  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor.
   *  Usually the next method is preferred.
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P,e:M#Element)(userCtx:UserContext[P,M],mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,p.Key)=>M#Key, run:P#Parser[M]=>Unit):M#Ret
    = p(p.binder(userCtx,(q:M#Parser)=>{e.parser=q; e},mapper,keyMapper)).invoke(run)*/
}