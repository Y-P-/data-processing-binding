package loader.core

object run {
  //intermediate declaration to satisfy the compiler.
  //but actually, m is contained within M#Launcher
  def exec[M<:loader.core.definition.Processor { type BaseParser>:P },
           P<:ParserBuilder { type BaseProcessor>:M }]
        (p:P,m:M)(l:m.Launcher)(init:l.type=>p.Parser[m.Kind,m.Ret]=>m.Element, mapper:(p.Elt[m.Kind,m.Ret],p.Kind)=>m.Kind, run:p.Parser[m.Kind,m.Ret]=>Unit):m.Ret = {
    p(p.binder[m.Kind,m.Ret](init(l),mapper)).invoke(run)
  }
  
  /** the most standard way to launch a pair of Parser/Processor.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p)(m)(_(ClassContext(classOf[Data.Top])),(e,s)=>s.toUpper,_.read(load("small"), "UTF-8"))
   */
  def apply[M<:loader.core.definition.Processor { type BaseParser>:P },
            P<:ParserBuilder { type BaseProcessor>:M }]
        (p:P, l:M#Launcher)                                                        //the parser and processor launcher
        (
            init:l.type => (p.Parser[l.proc.Kind,l.proc.Ret] => l.proc.Element),   //the parser initializer
            mapper:(p.Elt[l.proc.Kind,l.proc.Ret],p.Kind)=>l.proc.Kind,            //the mapper from the parser Kind to the processor Kind
            run:p.Parser[l.proc.Kind,l.proc.Ret]=>Unit                             //the parser runner
        ):
        l.proc.Ret
     = null.asInstanceOf[l.proc.Ret]
    //   exec[l.proc.type,p.type](p,l.proc)(l.myself)(init,mapper,run)
/*
  def include[M<:loader.core.definition.Def { type BaseParser>:P },
              P<:ParserBuilder { type BaseProcessor>:M }]
        (p:P, l:M#Element)(l1:l.proc.Element)                                //the parser and processor launcher
        (
            mapper:(p.Elt[l.proc.Kind,l.proc.Ret],p.Kind)=>l.proc.Kind,      //the mapper from the parser Kind to the processor Kind
            run:p.Parser[l.proc.Kind,l.proc.Ret]=>Unit                       //the parser runner
        ):
        l.proc.Ret
     = apply(p,l)(Any=>{(p:l.Parser)=>{l.parser=p; l1}},mapper,run)
     */
}