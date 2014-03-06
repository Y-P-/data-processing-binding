package loader.core
import loader.core.definition.Processor

object run {
  /** The nominal runner from scratch.
   *  Usually the next method is preferred.
   */
  def apply[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P)(init:p.Parser[M]=>M#Element, mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,P#Key)=>M#Key, run:p.Parser[M]=>Unit):M#Ret = {
    p(p.binder[M](init,mapper,keyMapper)).invoke(run)
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
            init:l.type => (p.Parser[M] => M#Element),    //the parser initializer
            mapper:(M#Element,P#Value)=>M#Value,          //the mapper from the parser Value to the processor Value
            keyMapper:(M#Element,P#Key)=>M#Key,           //the mapper from the parser Value to the processor Value
            run:p.Parser[M]=>Unit                         //the parser runner
        ):
        M#Ret
     = apply[P,M](p)(init(l),mapper,keyMapper,run)

  /** The nominal runner on an existing element, i.e. this includes the new parser in the current processor.
   *  Usually the next method is preferred.
   */
  def include[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P,e:M#Element)(mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,p.Key)=>M#Key, run:P#Parser[M]=>Unit):M#Ret
    = p(p.binder((q:M#Parser)=>{e.parser=q; e},mapper,keyMapper)).invoke(run)

  
  class Mapper[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton] {
    def apply(elt:M#Elt):EltMapper = new EltMapper(elt)
    class EltMapper(elt:M#Elt) {
      def mapVal(v:P#Value):M#Value = null
      def mapKey(v:P#Key):M#Key     = null
    }    
  }

  def apply1[P<:ParserBuilder { type BaseProcessor>:M }, M<:Processor with Singleton]
        (p:P)(init:p.Parser[M]=>M#Element, mapper:(M#Element,P#Value)=>M#Value, keyMapper:(M#Element,P#Key)=>M#Key, run:p.Parser[M]=>Unit):M#Ret = {
    val map = new Mapper[p.type,M]
    val f1 = (e:M#Elt,v:p.Value)=>map(e).mapVal(v)
    p(p.binder[M](init,f1,keyMapper)).invoke(run)
  }

}