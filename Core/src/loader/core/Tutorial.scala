/**
 *  This object highlights some of the patterns that are hidden within the general code.
 *  It has been quite simplified to make things a bit clearer.
 */
object Tutorial extends App {

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
     type Val
     type Sup<:P                        //we want to see P as a Sup (i.e. have access to more that casual P information => tighter coupling)
     abstract class E {                 //the inner class with the logic
       type S<:Sup with Singleton       //actual class for Sup. Cannot be declared generic (leads to illegal selection from volatile type); Singleton warrants type selection consistency (S#Val)
       val data:S#E                     //the cross referenced data (in P#E, we have a Q#E, in Q#E we have a P#E)
     }
     private class E1[X<:Sup with Singleton](pf: E=>X#E) extends E { val data=pf(this); type S=X }  //shows how to define a concrete class
     def top[X<:Sup with Singleton](pf: E=>X#E):E{type S=X} = new E1[X](pf)                       //one of the two necessary factory
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
    type UBase[-P<:BaseProcessor]<:UsrCtx[self.type,P]
    type MyImpl<:Impl
    trait Impl { this:MyImpl =>                          //see E above
      println("building basic parser")
      type Proc <: BaseProcessor with Singleton          //see S above
      type UC <: UBase[Proc]                             //the user data is precise in ParserBuilder kind, but only seen as BaseProcessor in Processor kind
      val userCtx:UC                                     //the exact type provided by the user
      val top:Proc#Elt                                   //see data above
      def push(v:Val)  = top.push(userCtx(top)(v))       //we can convert from ParserBuilder#Val to Processor#Val using userCtx
      def invoke(f:this.type=>Unit):Proc#Ret = top.run(f(this))
      def runIt:Unit = ()
    }
    //a concrete implementation
    abstract class Parser[X<:BaseProcessor with Singleton](pf: MyImpl=>X#Elt,val userCtx:UBase[X]) extends Impl { this:MyImpl=> val top=pf(this); type Proc=X; type UC=UBase[X] }
    //the factory method we are interested in
    def top[X<:BaseProcessor with Singleton](u:UBase[X],pf: MyImpl=>X#Elt):MyImpl{type Proc=X} = new Parser(pf,u)
  }
  trait Processor {self=>
    type Val
    type Ret
    type BaseParser<:ParserBuilder
    type UBase[-P<:BaseParser]<:UsrCtx[P,self.type]
    trait Elt {
      println("building basic processor")
      type Parser <: BaseParser with Singleton
      type UC <: UBase[Parser]
      val userCtx:UC
      val parser:Parser#Impl
      val eltCtx = userCtx(this)                         //we cannot use this efficiently in ParserBuilder#Impl for specific typing on Processor kind
      def push(v:Val):Unit = println(v)
      def run(f: =>Unit):Ret = null.asInstanceOf[Ret]
    }
    //a concrete implementation
    private class Element[X<:BaseParser with Singleton](val parser:X#Impl,val userCtx:UBase[X]) extends Elt { type Parser=X; type UC=UBase[X] }
    //the dual factory method we are interested in
    def builder[X<:BaseParser with Singleton](u:UBase[X]):X#Impl=>Elt{type Parser=X} = px => new Element(px,u)
  }
  //the factory; which returns a ParserBuilder#Impl
  def join[P<:ParserBuilder { type BaseProcessor>:Q },Q<:Processor { type BaseParser>:P }](p:P,q:Q)(u:p.UBase[q.type] with q.UBase[p.type]):p.Impl { type Proc=q.type } =
    p.top[q.type](u,q.builder[p.type](u))
  
  /**
   * A global coupling data structure.
   */
  class UsrCtx[-p<:ParserBuilder,-m<:Processor] {
    protected[this] type P = p
    protected[this] type M = m
    def apply(e:m#Elt):X = new X
    protected[this] class X {
      def apply(x:p#Val):m#Val = x.asInstanceOf[m#Val]
    }
  }
  
  //CASE 1: have parser <-> processor work together with a generic UsrCtx
  object parser extends ParserBuilder {self=>
    type Val = Int
    type BaseProcessor = Processor
    type UBase[-P<:BaseProcessor]=UsrCtx[self.type,P]
    class Parser[X<:BaseProcessor with Singleton](pf: MyImpl=>X#Elt,val userCtx:UBase[X]) extends Impl { this:MyImpl=> val top=pf(this); type Proc=X; type UC=UBase[X] }
    def top[X<:BaseProcessor with Singleton](u:UBase[X],pf: MyImpl=>X#Elt):MyImpl{type Proc=X} = new Parser(pf,u)
  }
  object processor extends Processor {self=>
    type Val = String
    type Ret = Double
    type BaseParser = ParserBuilder
    type UBase[-P<:BaseParser]=UsrCtx[P,self.type]
  }
  object uCtx extends UsrCtx[ParserBuilder { type Val=Int }, Processor { type Val=String }] {
    override def apply(e:M#Elt):X = new X
    protected[this] class X extends super.X {
      override def apply(x:P#Val):M#Val = if (x.isInfinite) "inf" else x.toString
    }
  }
  
  //CASE 2: have parser <-> processor work together with a specific UsrCtx and a specific implementation
  object parser1 extends ParserBuilder {self=>
    type Val = Int
    type BaseProcessor = Processor
    type UBase[-P<:BaseProcessor]=UsrCtx[self.type,P] with UCtx1
    trait Impl extends super.Impl {
      println(userCtx.hello+" parser")
    }
    //a concrete implementation
    private class Parser[X<:BaseProcessor with Singleton](pf: Impl=>X#Elt,val userCtx:UBase[X]) extends Impl { val top=pf(this); type Proc=X; type UC=UBase[X] }
    //the factory method we are interested in
  //  override def top[X<:BaseProcessor with Singleton](u:UBase[X],pf: Impl=>X#Elt):Impl{type Proc=X} = new Parser(pf,u)
  }
  object processor1 extends Processor { self=>
    type Val = String
    type Ret = Double
    type BaseParser = ParserBuilder
    type UBase[-P<:BaseParser]=UsrCtx[P,self.type] with UCtx1
    trait Elt extends super.Elt{
      println(userCtx.hello+" processor")
    }
    //a concrete implementation
    private class Element[X<:BaseParser with Singleton](val parser:X#Impl,val userCtx:UBase[X]) extends Elt { type Parser=X; type UC=UBase[X] }
    //the dual factory method we are interested in
    override def builder[X<:BaseParser with Singleton](u:UBase[X]):X#Impl=>Elt{type Parser=X} = px => new Element(px,u)
  }
  class UCtx1 extends UsrCtx[ParserBuilder { type Val=Int }, Processor { type Val=String }] {
    override def apply(e:M#Elt):X = new X
    val hello = "hello"
    protected[this] class X extends super.X {
      override def apply(x:P#Val):M#Val = if (x.isInfinite) "inf" else x.toString
    }
  }
  
  println("started")
  println(
      join(parser,processor)(uCtx).invoke(_.runIt) + join(parser1,processor1)(new UCtx1).invoke(_.runIt)
  )
  println("finished")
}
