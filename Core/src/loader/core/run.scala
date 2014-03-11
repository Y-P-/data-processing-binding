package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal runner from scratch.
   *  Usually the next method is preferred.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }, U<:UsrCtx[P,M]]
        (p:P,m:M,u:U)(l:m.Launcher)(init:(l.type,U)=>p.Impl=>m.Elt{type Parser=P;type UC=U}):Unit = {
    p.top[m.type,U](u,init(l,u))
  }
  
  /** The most standard way to launch a pair of Parser/Processor.
   *  Here, we use the fact that the Processor is contained within a launcher to remove one parameter.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p,l)(_(ClassContext(classOf[Data.Top])),(e,s)=>s.toUpper,_.read(load("small"), "UTF-8"))
   *  Note that the function apply(p,l) _ will produce an interesting binding between a parser and a processor.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }, U<:UsrCtx[P,M]] (p:P, l:M#Launcher)                                                        //the parser and processor launcher
        (
            userCtx:U,
            init:(l.type,U) => (l.type,U)=>p.Impl=>l.proc.Elt{type Parser=P;type UC=U}
        ):Unit
     = apply[P,M,U](p,l.proc,userCtx)(l.myself)(init)
/*
  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor.
   *  Usually the next method is preferred.
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P,e:M#Element)(userCtx:UserContext[P,M],mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,p.Key)=>M#Key, run:P#Parser[M]=>Unit):M#Ret
    = p(p.binder(userCtx,(q:M#Parser)=>{e.parser=q; e},mapper,keyMapper)).invoke(run)*/
}