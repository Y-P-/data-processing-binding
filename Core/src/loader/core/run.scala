package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal runner from scratch.
   *  Usually the next method is preferred.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P)(userCtx:UserContext[P,M], init:p.Parser[M]=>M#Element, mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,P#Key)=>M#Key, run:p.Parser[M]=>Unit):M#Ret = {
    p(p.binder[M](userCtx,init,mapper,keyMapper)).invoke(run)
  }
  
  /** The most standard way to launch a pair of Parser/Processor.
   *  Here, we use the fact that the Processor is contained within a launcher to remove one parameter.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p,l)(_(ClassContext(classOf[Data.Top])),(e,s)=>s.toUpper,_.read(load("small"), "UTF-8"))
   *  Note that the function apply(p,l) _ will produce an interesting binding between a parser and a processor.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton](p:P, l:M#Launcher)                                                        //the parser and processor launcher
        (
            userCtx:UserContext[P,M],
            init:l.type => (p.Parser[M] => M#Element),    //the parser initializer
            mapper:(M#Element,P#Value)=>M#Value,          //the mapper from the parser Value to the processor Value
            keyMapper:(M#Element,P#Key)=>M#Key,           //the mapper from the parser Value to the processor Value
            run:p.Parser[M]=>Unit                         //the parser runner
        ):
        M#Ret
     = apply[P,M](p)(userCtx,init(l),mapper,keyMapper,run)

  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor.
   *  Usually the next method is preferred.
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P,e:M#Element)(userCtx:UserContext[P,M],mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,p.Key)=>M#Key, run:P#Parser[M]=>Unit):M#Ret
    = p(p.binder(userCtx,(q:M#Parser)=>{e.parser=q; e},mapper,keyMapper)).invoke(run)
}