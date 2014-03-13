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
   * 
   * In our case, their is a second disymmetry: one side (the parser) is controlling the other (the processor) ; the
   * invocation flow is thus not symmetric. This is why this pattern is not exactly useful, except as a starting base.
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
   *       The actual implementation is a bit more complex because of the introduction of Delegates,
   *       a subclass that the user actually wants to use
   */
  trait ParserBuilder {self=>
    type Val                                             //a type specific in ParserBuilder
    type BaseProcessor<:Processor                        //see Sup above
    type UBase[-P<:BaseProcessor]<:UsrCtx[self.type,P]                            //the UsrCtx class relevant to that parser
    protected type Impl[X<:BaseProcessor with Singleton]<:Parser { type Proc=X }  //the actual parser class
    type Parser<:BaseImpl                                //we cannot work on Parser[X] because X is inside; this will be used for covariance
    trait BaseImpl { this:Parser=>                       //see E above
      println("building basic parser")
      type Proc <: BaseProcessor with Singleton          //see S above
      type UC = UBase[Proc]                              //we have to follow Proc to access the correct types in Proc
      val userCtx:UC                                     //the exact type provided by the user
      val top:Proc#Elt                                   //see data above; actual declaration can only be done in actual class and will have to be lazy (the Impl[X]=>X#Elt function can only be called in the bottom class without resorting to a cast)
      var cur = top                                      //there are sometimes problems in getting back the right Elt kind ; check this
      def push(v:Val)  = cur=cur.push(userCtx(cur)(v))   //we can convert from ParserBuilder#Val to Processor#Val using userCtx, we can follow Elt.
      def invoke(f:this.type=>Unit):Proc#Ret = top.run(f(this))
      def runIt():Unit = ()
    }
    //the factory method we are interested in
    def top[X<:BaseProcessor with Singleton](u:UBase[X],pf: Impl[X]=>X#Elt):Impl[X]
  }
  trait Processor {self=>
    type Val
    type Ret
    type BaseParser<:ParserBuilder
    type UBase[-P<:BaseParser]<:UsrCtx[P,self.type]
    protected type Element[X<:BaseParser with Singleton] <: Elt { type Parser=X }
    type Elt <: BaseElt
    trait BaseElt { this:Elt =>
      println("building basic processor")
      type Parser <: BaseParser with Singleton
      type UC = UBase[Parser]
      val userCtx:UC
      val parser:Parser#Parser
      val eltCtx = userCtx(this)                         //we cannot use this efficiently in ParserBuilder#Impl for specific typing on Processor kind
      def push(v:Val):Elt = { println(v); this }
      def run(f: =>Unit):Ret = { f; null.asInstanceOf[Ret] }
    }
    def builder[X<:BaseParser with Singleton](u:UBase[X]):X#Parser=>Element[X]
  }
  //the factory; which returns a ParserBuilder#Impl
  def join[P<:ParserBuilder { type BaseProcessor>:Q },Q<:Processor { type BaseParser>:P }](p:P,q:Q)(u:p.UBase[q.type] with q.UBase[p.type]):p.Parser { type Proc=q.type } =
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
  
  /*************************************************************************
   *  CASE 1: have parser <-> processor work together with a generic UsrCtx
   *          this is the basic test that shows that elementary things are
   *          compiling and working
   *************************************************************************/
  class P0 extends ParserBuilder {
    type Val = Int
    type BaseProcessor = Processor
    type UBase[-P<:BaseProcessor]=UsrCtx[this.type,P]
    type Parser = BaseImpl
    protected class Impl[X<:BaseProcessor with Singleton](pf: Impl[X]=>X#Elt,val userCtx:UBase[X]) extends BaseImpl { lazy val top=pf(this); type Proc=X; override def runIt()=println(push(3)) }
    def top[X<:BaseProcessor with Singleton](u:UBase[X],pf: Impl[X]=>X#Elt):Impl[X] = new Impl[X](pf,u)
  }
  class M0 extends Processor {
    type Val = String
    type Ret = Double
    type BaseParser = ParserBuilder
    type UBase[-P<:BaseParser]=UsrCtx[P,this.type]
    type Elt = BaseElt
    class Element[X<:BaseParser with Singleton](val parser:X#Parser,val userCtx:UBase[X]) extends BaseElt { type Parser=X }
    def builder[X<:BaseParser with Singleton](u:UBase[X]):X#Parser=>Element[X] = new Element(_,u)
  }
  object uCtx extends UsrCtx[ParserBuilder { type Val=Int }, Processor { type Val=String }] {
    override def apply(e:M#Elt):X = new X
    protected[this] class X extends super.X {
      override def apply(x:P#Val):M#Val = if (x.isInfinite) "inf" else x.toString
    }
  }
  
  /*************************************************************************
   *  CASE 2: have parser <-> processor work together with a custom UsrCtx
   *          this is superior test that shows that advanced things are
   *          working: i.e. custom parsers/processors
   *************************************************************************/
  class P1 extends ParserBuilder {
    type Val = Int
    type BaseProcessor = Processor
    type UBase[-P<:BaseProcessor]=UsrCtx[this.type,P] with UCtx1
    trait Parser extends super.BaseImpl { this:Parser=>
      println(userCtx.hello+" parser")
    }
    protected class Impl[X<:BaseProcessor with Singleton](pf: Impl[X]=>X#Elt,val userCtx:UBase[X]) extends Parser { lazy val top=pf(this); type Proc=X }
    def top[X<:BaseProcessor with Singleton](u:UBase[X],pf: Impl[X]=>X#Elt):Impl[X] = new Impl[X](pf,u)
  }
  class M1 extends Processor {
    type Val = String
    type Ret = Double
    type BaseParser = ParserBuilder
    type UBase[-P<:BaseParser]=UsrCtx[P,this.type] with UCtx1
    trait Elt extends super.BaseElt{
      println(userCtx.hello+" processor")
    }
    protected class Element[X<:BaseParser with Singleton](val parser:X#Parser,val userCtx:UBase[X]) extends Elt { type Parser=X }
    def builder[X<:BaseParser with Singleton](u:UBase[X]):X#Parser=>Element[X] = new Element(_,u)
  }
  class UCtx1 extends UsrCtx[ParserBuilder { type Val=Int }, Processor { type Val=String }] {
    override def apply(e:M#Elt):X = new X
    val hello = "hello"
    protected[this] class X extends super.X {
      override def apply(x:P#Val):M#Val = if (x.isInfinite) "*inf*" else s"*${x.toString}*"
    }
  }
  
  //testing
  println("started")
  val r1 = join(new P0,new M0)(uCtx).invoke(_.runIt)
  join(new P1,new M1)(new UCtx1).invoke(_.runIt)
  join(new P1,new M0)(new UCtx1).invoke(_.runIt)
  join(new P0,new M1)(new UCtx1).invoke(_.runIt)
  println("finished")
  //conclusion:
  // 1) both parser and processor can be used with each other: that's expected because they share appropriate bases
  // 2) when using a 1 version, then UCtx1 has to be used ; correct
  // 3) the correct return type is inferred
  // 4) generic types are correctly inferred (no need to indicate them to help the compiler)
  // 5) transcoding from Int to String is OK ; it comes from UsrCtx
  // 6) specific UserCtx works
}
