package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal runner from scratch */
  //def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }]
  def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor]
        (p:P,m:M)(init:p.Parser[m.Key,m.Kind,m.Ret]=>m.Element, mapper:(p.Elt[m.Key,m.Kind,m.Ret],p.Value)=>m.Kind, keyMapper:(p.Elt[m.Key,m.Kind,m.Ret],p.Key)=>m.Key, run:p.Parser[m.Key,m.Kind,m.Ret]=>Unit):m.Ret = {
    p(p.binder[m.Key,m.Kind,m.Ret](init,mapper,keyMapper)).invoke(run)
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
            init:l.type => (p.Parser[l.proc.Key,l.proc.Kind,l.proc.Ret] => l.proc.Element),   //the parser initializer
            mapper:(p.Elt[l.proc.Key,l.proc.Kind,l.proc.Ret],p.Value)=>l.proc.Kind,           //the mapper from the parser Kind to the processor Kind
            keyMapper:(p.Elt[l.proc.Key,l.proc.Kind,l.proc.Ret],p.Key)=>l.proc.Key,           //the mapper from the parser Kind to the processor Kind
            run:p.Parser[l.proc.Key,l.proc.Kind,l.proc.Ret]=>Unit                             //the parser runner
        ):
        l.proc.Ret
     = apply(p,l.proc)(init(l),mapper,keyMapper,run)

  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor */
  def include[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }]
        (p:P,m:M)(e:m.Element, mapper:(p.Elt[m.Key,m.Kind,m.Ret],p.Value)=>m.Kind, keyMapper:(p.Elt[m.Key,m.Kind,m.Ret],p.Key)=>m.Key, run:p.Parser[m.Key,m.Kind,m.Ret]=>Unit):m.Ret
    = p(p.binder[m.Key,m.Kind,m.Ret](p=>{e.parser=p; e},mapper,keyMapper)).invoke(run)

  
  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor */
  def include1[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor { type BaseParser>:P }]
        (p:P,e:M#Elt)(mapper:(p.Elt[e.proc.Key,e.proc.Kind,e.proc.Ret],p.Value)=>e.proc.Kind, keyMapper:(p.Elt[e.proc.Key,e.proc.Kind,e.proc.Ret],p.Key)=>e.proc.Key, run:p.Parser[e.proc.Key,e.proc.Kind,e.proc.Ret]=>Unit):e.proc.Ret
    = include(p,e.proc)(e.myself,mapper,keyMapper,run)

}