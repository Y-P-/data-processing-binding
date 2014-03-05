package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal runner from scratch.
   *  Usually the next method is preferred.
   */
  //def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }]
  def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor]
        (p:P,m:M)(init:p.Parser[m.Key,m.Value,m.Ret]=>m.Element, mapper:(p.Elt[m.Key,m.Value,m.Ret],p.Value)=>m.Value, keyMapper:(p.Elt[m.Key,m.Value,m.Ret],p.Key)=>m.Key, run:p.Parser[m.Key,m.Value,m.Ret]=>Unit):m.Ret = {
    p(p.binder[m.Key,m.Value,m.Ret](init,mapper,keyMapper)).invoke(run)
  }
  
  /** The most standard way to launch a pair of Parser/Processor.
   *  Here, we use the fact that the Processor is contained within a launcher to remove one parameter.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p,l)(_(ClassContext(classOf[Data.Top])),(e,s)=>s.toUpper,_.read(load("small"), "UTF-8"))
   *  Note that the function apply(p,l) _ will produce an interesting binding between a parser and a processor.
   */
  //def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }](p:P, l:M#Launcher)                                                        //the parser and processor launcher
  def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor](p:P, l:M#Launcher)                                                        //the parser and processor launcher
        (
            init:l.type => (p.Parser[l.proc.Key,l.proc.Value,l.proc.Ret] => l.proc.Element),   //the parser initializer
            mapper:(p.Elt[l.proc.Key,l.proc.Value,l.proc.Ret],p.Value)=>l.proc.Value,           //the mapper from the parser Value to the processor Value
            keyMapper:(p.Elt[l.proc.Key,l.proc.Value,l.proc.Ret],p.Key)=>l.proc.Key,           //the mapper from the parser Value to the processor Value
            run:p.Parser[l.proc.Key,l.proc.Value,l.proc.Ret]=>Unit                             //the parser runner
        ):
        l.proc.Ret
     = apply(p,l.proc)(init(l),mapper,keyMapper,run)

  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor.
   *  Usually the next method is preferred.
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }]
        (p:P,m:M)(e:m.Element, mapper:(p.Elt[m.Key,m.Value,m.Ret],p.Value)=>m.Value, keyMapper:(p.Elt[m.Key,m.Value,m.Ret],p.Key)=>m.Key, run:p.Parser[m.Key,m.Value,m.Ret]=>Unit):m.Ret
    = p(p.binder[m.Key,m.Value,m.Ret](p=>{e.parser=p; e},mapper,keyMapper)).invoke(run)

  
  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor */
  def include[P<:ParserBuilder { type BaseProcessor>:E#Proc }, E<:(Processor { type BaseParser>:P })#Elt]
        (p:P,e:E)(mapper:(p.Elt[e.proc.Key,e.proc.Value,e.proc.Ret],p.Value)=>e.proc.Value, keyMapper:(p.Elt[e.proc.Key,e.proc.Value,e.proc.Ret],p.Key)=>e.proc.Key, run:p.Parser[e.proc.Key,e.proc.Value,e.proc.Ret]=>Unit):e.proc.Ret
    = include(p,e.proc)(e.myself,mapper,keyMapper,run)

}