package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal runner from scratch */
  def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }]
        (p:P,m:M)(init:p.Parser[m.Kind,m.Ret]=>m.Element, mapper:(p.Elt[m.Kind,m.Ret],p.Kind)=>m.Kind, run:p.Parser[m.Kind,m.Ret]=>Unit):m.Ret = {
    p(p.binder[m.Kind,m.Ret](init,mapper)).invoke(run)
  }
  
  /** The most standard way to launch a pair of Parser/Processor.
   *  Here, we use the fact that the Processor is contained within a launcher to remove one parameter.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p,l)(_(ClassContext(classOf[Data.Top])),(e,s)=>s.toUpper,_.read(load("small"), "UTF-8"))
   *  Note that the function apply(p,l) _ will produce an interesting binding between a parser and a processor.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }](p:P, l:M#Launcher)                                                        //the parser and processor launcher
        (
            init:l.type => (p.Parser[l.proc.Kind,l.proc.Ret] => l.proc.Element),   //the parser initializer
            mapper:(p.Elt[l.proc.Kind,l.proc.Ret],p.Kind)=>l.proc.Kind,            //the mapper from the parser Kind to the processor Kind
            run:p.Parser[l.proc.Kind,l.proc.Ret]=>Unit                             //the parser runner
        ):
        l.proc.Ret
     = apply(p,l.proc)(init(l),mapper,run)

  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor */
  def include[P<:ParserBuilder { type BaseProcessor>:M },M<:Processor { type BaseParser>:P }]
        (p:P,m:M)(e:m.Element, mapper:(p.Elt[m.Kind,m.Ret],p.Kind)=>m.Kind, run:p.Parser[m.Kind,m.Ret]=>Unit):m.Ret
    = p(p.binder[m.Kind,m.Ret](p=>{e.parser=p; e},mapper)).invoke(run)

  
  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor */
  def include[M<:Processor { type BaseParser>:P }, P<:ParserBuilder { type BaseProcessor>:M }]
        (p:P,e:M#Element)(mapper:(p.Elt[e.proc.Kind,e.proc.Ret],p.Kind)=>e.proc.Kind, run:p.Parser[e.proc.Kind,e.proc.Ret]=>Unit):e.proc.Ret
    = include(p,e.proc)(e.myself,mapper,run)

}