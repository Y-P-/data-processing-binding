package loader.core

object run {
  //intermediate declaration to satisfy the compiler.
  //but actually, m is contained within M#Launcher
  protected def exec[M<:loader.core.definition.Def { type BaseParser>:P },
           P<:ParserBuilder { type BaseProcessor>:M }]
        (p:P)(m:M)(l:M#Launcher)(init:l.type=>p.Parser[m.Kind,m.Ret]=>m.Element, mapper:(p.Elt[m.Kind,m.Ret],p.Kind)=>m.Kind, run:p.Parser[m.Kind,m.Ret]=>Unit):m.Ret = {
    p(p.binder[m.Kind,m.Ret](init(l),mapper)).invoke(run)
  }
  
  /** the most standard way to launch a pair of Parser/Processor.
   *  Example:
   *     val p = new parsers.Struct(256,40,false)
   *     val m = motors.Struct.ctx(out,2,userCtx)
   *     apply(p)(m)(_(ClassContext(classOf[Data.Top])),(e,s)=>s.toUpper,_.read(load("small"), "UTF-8"))
   */
  def apply[M<:loader.core.definition.Def { type BaseParser>:P },
            P<:ParserBuilder { type BaseProcessor>:M }]
        (p:P)                                                                //the parser
        (l:M#Launcher)                                                       //the processor launcher
        (
            init:l.type=>p.Parser[l.proc.Kind,l.proc.Ret]=>l.proc.Element,   //the parser initializer
            mapper:(p.Elt[l.proc.Kind,l.proc.Ret],p.Kind)=>l.proc.Kind,      //the mapper from the parser Kind to the processor Kind
            run:p.Parser[l.proc.Kind,l.proc.Ret]=>Unit                       //the parser runner
        ):
        l.proc.Ret
     = exec[l.Proc,p.type](p)(l.proc)(l)(init,mapper,run)


}