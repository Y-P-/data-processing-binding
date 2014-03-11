/**
 *  This object highlights some of the patterns that are hidden within the general code.
 *  It has been quite simplified to make things a bit clearer.
 */
object PatternBase {

  /**
   * Generic pattern for cross referencing two hierarchies.
   * The real object which is being the target of the pattern is E.
   * Each E from a hierarchy (P or Q) points to an associated value in the other hierarchy (tight association.)
   * Furthermore, we want to be able to refine the definitions by having tighter coupling if necessary (type Sup)
   * 
   * There is a symmetry in the base pattern when both method in top and builder are effectively defined in both classes.
   * This pattern in itself is limited in use. The next example will highlight the actual pattern used in the code, with
   * the symmetry being broken, but also with additional properties.
   */
  trait Cross[+This<:Cross[This,P],+P<:Cross[P,This]] { self:This=>
     type Sup<:P                        //we want to see P as a Sup (i.e. have access to more that casual P information => tighter coupling)
     abstract class E {                 //the inner class with the logic
       type S<:Sup with Singleton       //the actual class for Sup. It is actually never used: the important part here is Singleton, which warrants consistence
       val data:S#E                     //the cross referenced data (in P#E, we have a Q#E, in Q#E we have a P#E)
     }
     private class E1[X<:Sup with Singleton](pf: E=>X#E) extends E { val data=pf(this); type S=X }  //shows how to define a concrete class
     def top[X<:Sup with Singleton](pf: E=>X#E):E = new E1[X](pf)                                   //one of the two necessary factory
     def builder[X<:Sup with Singleton]:X#E=>E = px => new E1[X](_=>px)                             //the second mandatory factory
  }
  def join1[P<:Cross[P,Q] { type Sup>:Q },Q<:Cross[Q,P]{ type Sup>:P }](p:P,q:Q):p.E =              //a global factory for P#E
    p.top[q.type](q.builder[p.type])
  def join2[P<:Cross[P,Q] { type Sup>:Q },Q<:Cross[Q,P]{ type Sup>:P }](q:Q,p:P):q.E =              //a global factory for Q#E
    q.top[p.type](p.builder[q.type])


  /**
   * Generic pattern used for cross referencing ParserBuilder/Processor.
   * We break the symmetry because ParserBuilder is the object that will get to be activated: only one join (above)
   * is of any real use, so there is no point in defining useless methods.
   * Furthermore, we add in the UsrCtx external data, which itself breaks the symmetry (parameter order.)
   * Note: Having tried other patterns, this is the less constraining I found. Other experiments provided tighter,
   *       unnecessary coupling between ParserBuilder and Processor.
   *       The small cost is not being able to write top.push(top.eltCtx(v)) which is less expensive.
   */
  trait ParserBuilder {self=>
    type Val                                             //a type specific in ParserBuilder
    type BaseProcessor<:Processor                        //see Sup above
    trait Impl {                                         //see E above
      type Proc <: BaseProcessor with Singleton          //see S above
      type UC <: UsrCtx[self.type,Proc]                  //the user data is precise in ParserBuilder kind, but only seen as BaseProcessor in Processor kind
      val userCtx:UC                                     //the exact type provided by the user
      val top:Proc#Elt                                   //see data above
      def push(v:Val)  = top.push(userCtx(top)(v))       //we can convert from ParserBuilder#Val to Processor#Val using userCtx
    }
    //a concrete implementation
    private class Parser[X<:BaseProcessor with Singleton,U<:UsrCtx[self.type,X]](pf: Impl=>X#Elt,val userCtx:U) extends Impl { val top=pf(this); type Proc=X; type UC=U }
    //the factory method we are interested in
    def top[X<:BaseProcessor with Singleton,U<:UsrCtx[self.type,X]](u:U,pf: Impl=>X#Elt{type Builder>:self.type}):Impl = new Parser(pf,u)
  }
  trait Processor {self=>
    type Val
    type BaseParser<:ParserBuilder
    trait Elt {
      type Builder <: BaseParser with Singleton
      type UC <: UsrCtx[Builder,self.type]
      val userCtx:UC
      val parser:Builder#Impl
      val eltCtx = userCtx(this)                         //we cannot use this efficiently in ParserBuilder#Impl for specific typing on Processor kind
      def push(v:Val):Unit = println(v)
    }
    //a concrete implementation
    private class Element[X<:BaseParser with Singleton,U<:UsrCtx[X,self.type]](val parser:X#Impl,val userCtx:U) extends Elt { type Builder=X; type UC=U }
    //the dual factory method we are interested in
    def builder[X<:BaseParser with Singleton,U<:UsrCtx[X,self.type]](u:U):X#Impl=>Elt{type Builder=X} = px => new Element(px,u)
  }
  //the factory; which returns a ParserBuilder#Impl
  def join[P<:ParserBuilder { type BaseProcessor>:Q },Q<:Processor { type BaseParser>:P },U<:UsrCtx[P,Q]](p:P,q:Q,u:U):p.Impl =
    p.top[q.type,U](u,q.builder[p.type,U](u))
  
  /**
   * A global coupling data structure.
   */
  class UsrCtx[-p<:ParserBuilder,-m<:Processor] {
    def apply(e:m#Elt):X = new X
    protected[this] class X {
      def apply(x:p#Val):m#Val = x.asInstanceOf[m#Val]
    }
  }
}
