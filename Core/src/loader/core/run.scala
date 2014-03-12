package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal builder from scratch. Usually the next method is preferred (one less parameter.)
   *  This builds a parser implementation ready to be run.
   */
  //def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }, U<:UsrCtx[P,M]]
  //      (p:P,m:M,u:U)(l:m.Launcher)(init:(l.type,U)=>p.Impl{type Proc=m.type;type UC=U}=>m.Elt{type Parser=p.type;type UC=U}):p.Impl{type Proc=m.type;type UC=U}
  // = null// = p.top[m.type,U](u,init(l,u))

  
  /** The most standard way to launch a pair of Parser/Processor.
   *  Here, we use the fact that the Processor is contained within a launcher to remove one parameter.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p,l,u)(_(ClassContext(classOf[Data.Top])))
   */
 // def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }, U<:UsrCtx[P,M]]
 //       (p:P,l:M#Launcher,u:U)(init:(l.type,U)=>p.Impl{type Proc=l.proc.type}=>l.proc.Elt{type Parser=P;type UC=U}):p.Impl{type Proc=l.proc.type}
 //   = null//p.top[l.proc.type,U](u,init(l,u))
/*
  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor.
   *  Usually the next method is preferred.
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P,e:M#Element)(userCtx:UserContext[P,M],mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,p.Key)=>M#Key, run:P#Parser[M]=>Unit):M#Ret
    = p(p.binder(userCtx,(q:M#Parser)=>{e.parser=q; e},mapper,keyMapper)).invoke(run)*/
}