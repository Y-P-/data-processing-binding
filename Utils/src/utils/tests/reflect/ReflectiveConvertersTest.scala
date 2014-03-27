package utils.tests.reflect

import org.junit.Test
import java.io.PrintWriter
import scala.reflect.ClassTag
import utils.LogTester._
import utils.reflect._

/** Here we test converters as we need them.
 *  Scenario: at some point we have built an instance of U (likely spawned through reflexion)
 *  The expected data is V and we need a conversion:
 *  a) U contains a V builder with the required parameters (ConvertData,Processor#EltBase) => find and invoke
 *  b) the conversion is provided by an helper class W (could be V itself)
 *     - we don't want to have to clutter U with references to that class
 *     - this leaves the only possibilities that W is first rate (conversion = 'static' method)
 *       or that we can somehow provide an instance of W to work on.
 */
object ReflectiveConvertersTest {

  abstract class WithCheck {
    def n:Int
    WithCheck(n,this)
    def doNothing() = ()
  }
  object WithCheck {
    var count=0
    def apply(n:Int,o:Object) = {
      count+=1
      if (count!=n) throw new IllegalStateException(s"an object has more than one representation ($count): $o")
    }
  }
  //used to test various nesting cases ; stable path using val are NOT supported (no way to get back to the val through Java reflection)
  //Note that we check that the object is spawned once only
  object A {
    WithCheck(3,this)
    object AA {
      WithCheck(2,this)
      val parent = A  //force reference to ensure init (count check)
      class X {
        def cv(x:Integer):String = x.toString
      }
      object Y {
        WithCheck(1,this)
        val parent = AA  //force reference to ensure init (count check)
        def cv(x:Integer):String = x.toString
      }
      class Z(fd:ConvertData) {
        def cv(x:Integer):String = x.toString
      }
    }
  }
  val a=A.AA.Y.cv(4) //forces object init ; done to ensure count is right
  //same, but with a full fledged class/object pair, with an object part that inherits from another class
  class A1 {
    def doNothing() = ()
  }
  object A1 extends WithCheck {
    def n=6
    object AA extends WithCheck {
      def n=5
      val parent = A1
      class X {
        def cv(x:Integer):String = x.toString
      }
      object Y extends WithCheck {
        def n=4
        val parent = AA
        def cv(x:Integer):String = x.toString
      }
      class Z(fd:ConvertData) {
        def cv(x:Integer):String = x.toString
      }
    }
  }
  val a1=A1.AA.Y.cv(4)
  if (a1!=a) throw new IllegalStateException("ensure the code is executed by the JVM!")
  //basic object being converted to
  class V(out:PrintWriter)
  object V {
    def invoke(out:PrintWriter, n:Int) = { out.println(s"invoked V.toV$n"); new V(out) }
    def toV1(u:S,fd:ConvertData):V = invoke(u.out,1)
    def toV2(u:S):V = invoke(u.out,2)
  }
  
  //extensive check with derived classes
  class W(out:PrintWriter) extends V(out)
  object W {
    def invoke(out:PrintWriter, n:Int):W = { out.println(s"invoked W.toV$n"); new W(out) }
    def toV1(u:S,fd:ConvertData):V = invoke(u.out,1)
    def toV2(u:S):W = invoke(u.out,2)
    def toV3(u:T,fd:ConvertData):V = invoke(u.out,3)
    def toV4(u:T):W = invoke(u.out,4)
  }
  
  //basic object being converted from
  class S(val out:PrintWriter) {
    def invoke(n:Int) = { out.println(s"invoked S.toV$n"); new V(out) }
    def toV1(fd:ConvertData):V = invoke(1)
    def toV2():V = invoke(2)
  }
  
  //derived object to convert from
  class T(out:PrintWriter) extends S(out)
  class D0(val s:S)
  class D1(s:T) extends D0(s)
  
  class X0(fd:ConvertData) {
    def invoke(out:PrintWriter, n:Int):V = { out.println(s"invoked X0.toV$n"); new V(out) }
    def toV3(u:S):V = invoke(u.out,3)
    def toV4(u:T):V = invoke(u.out,4)
  }
  class X1(fd:ConvertData) extends X0(fd) {
    override def invoke(out:PrintWriter, n:Int):W = { out.println(s"invoked X1.toV$n"); new W(out) }
    def toV1(u:S):W = invoke(u.out,1)
    def toV2(u:T):W = invoke(u.out,2)
  }

  class Y0(fd:ConvertData) {
    def invoke(out:PrintWriter, n:Int):V = { out.println(s"invoked $n Y0.from${if(n==3)"X"else"Y"}"); new V(out) }
    def dummy(x:Int):V = null  //show that type matter: Int not seen as S or T
    def fromX(u:S):V = invoke(u.out,3)
    def fromY(u:T):V = invoke(u.out,4)
  }
  class Y1(fd:ConvertData) extends Y0(fd) {
    override def invoke(out:PrintWriter, n:Int):W = { out.println(s"invoked $n Y1.from${if(n==1)"X"else"Y"}"); new W(out) }
    override def dummy(x:Int):W = null
    override def fromX(u:S):W = invoke(u.out,1)
    override def fromY(u:T):W = invoke(u.out,2)
  }
  
  /** Test to verify that DataActors are correctly found */
  @Test class ReflexiveConverterTest extends StandardTester {
    val cS = classOf[S]
    val cV = classOf[V]
    val cT = classOf[T]
    val cW = classOf[W]
    val fd0 = ConvertData.empty
    def apply(file:Solver,out0:PrintWriter) = {
      def checkBase(in:Class[_],cS:Class[_>:T<:S],cV:Class[_>:W<:V],fld:String):Unit = try {
        val x = Converters(in, cS, cV, fld)
        out0.println(x)
        if (x!=None) x.get(fd0)(new T(out0)) //we invoke with T just to be sure we have no exception. It doesn't matter for the test itself.
      } catch {
        case e:Exception=>out0.println(e.getMessage)
      }
      def check0(in:Class[_],cS:Class[_>:T<:S],cV:Class[_>:W<:V],n:Int):Unit = checkBase(in,cS,cV,s"toV$n")
      def check(in:Class[_],n:Int) = check0(in,cS,cV,n)
      
      //checking that all converters are found in the source
      for (i <- 1 to 2) check(cS,i)
      //checking that all converters are found in the destination
      for (i <- 1 to 2) check(V.getClass,i)
      
      //checking sub-classes
      out0.println(Converters(cS, cS, cV, "toV1"))  //can find toV1 in source cS to convert from S to V
      out0.println(Converters(cT, cS, cV, "toV1"))  //cannot find toV1 (T is not S)
      out0.println(Converters(cT, cT, cV, "toV1"))  //can find toV1 now
      out0.println(Converters(cS, cT, cV, "toV2"))  //can find toV1 (T is an S) ; using toV2 to see we can find it too
      
      //checking constructors with derivation
      out0.println ("---- constructors ----")
      out0.println(Converters(classOf[D0], cS, classOf[D0], null)) //D0(S)
      out0.println(Converters(classOf[D0], cT, classOf[D0], null)) //D0(S)
      out0.println(Converters(classOf[D1], cS, classOf[D1], null)) //no
      out0.println(Converters(classOf[D1], cT, classOf[D1], null)) //D1(T)
      out0.println(Converters(classOf[D0], cS, classOf[D1], null)) //no (D0 > D1)
      out0.println(Converters(classOf[D0], cT, classOf[D1], null)) //no (D0 > D1)
      out0.println(Converters(classOf[D1], cS, classOf[D0], null)) //no (S > T)
      out0.println(Converters(classOf[D1], cT, classOf[D0], null)) //D1(T)
      
      //checking that all converters are found properly when using derived classes in various places
      out0.println ("---- S to V ----")
      for (i <- 1 to 4) check0(W.getClass,classOf[S],classOf[V],i)  //1,2
      out0.println ("---- T to W ----")
      for (i <- 1 to 4) check0(W.getClass,classOf[T],classOf[W],i)  //2,4
      out0.println ("---- S to W ----")
      for (i <- 1 to 4) check0(W.getClass,classOf[S],classOf[W],i)  //2
      out0.println ("---- T to V ----")
      for (i <- 1 to 4) check0(W.getClass,classOf[T],classOf[V],i)  //1,2,3,4
      
      //checking that we can find an appropriate converter when not giving a name
      //also used to check how the Reflect framework behaves given inheritance
      out0.println ("---- S to V in X1 instancied with fd0 ----")
      checkBase(classOf[X1], cS, cW, null) //V1
      checkBase(classOf[X1], cS, cV, null) //V1,V3 (no min)
      checkBase(classOf[X1], cT, cV, null) //V1,V2,V3,V4 (no min)
      checkBase(classOf[X1], cT, cW, null) //V1,V2 (min = V2)
      out0.println ("---- S to V in X0 instancied with fd0 ----")
      checkBase(classOf[X0], cS, cW, null) //no
      checkBase(classOf[X0], cS, cV, null) //V3
      checkBase(classOf[X0], cT, cV, null) //V3,V4 (min V4)
      checkBase(classOf[X0], cT, cW, null) //no

      //checking that we can find an appropriate converter with overloading
      //this is broadly a copy of the previous test series, with minor adaptations
      //some tests are likely redundant ; it doesn't really matter
      out0.println ("---- S to V in Y1 instancied with fd0 ----")
      checkBase(classOf[Y1], cS, cW, "fromX") //1
      checkBase(classOf[Y1], cS, cV, "fromX") //1
      checkBase(classOf[Y1], cT, cV, "fromX") //1
      checkBase(classOf[Y1], cT, cW, "fromX") //1
      out0.println ("---- S to V in Y0 instancied with fd0 ----")
      checkBase(classOf[Y0], cS, cW, "fromX") //no
      checkBase(classOf[Y0], cS, cV, "fromX") //3
      checkBase(classOf[Y0], cT, cV, "fromX") //3
      checkBase(classOf[Y0], cT, cW, "fromX") //no
      out0.println ("---- S to V in Y1 instancied with fd0 ----")
      checkBase(classOf[Y1], cS, cW, null) //1
      checkBase(classOf[Y1], cS, cV, null) //1
      checkBase(classOf[Y1], cT, cV, null) //1,2 (min 2)
      checkBase(classOf[Y1], cT, cW, null) //2
      out0.println ("---- S to V in Y0 instancied with fd0 ----")
      checkBase(classOf[Y0], cS, cW, null) //no
      checkBase(classOf[Y0], cS, cV, null) //3
      checkBase(classOf[Y0], cT, cV, null) //3,4 (min 4)
      checkBase(classOf[Y0], cT, cW, null) //no
      
      out0.println ("---- Standard Java classes, compatibility with static methods, classes without ConvertData, deep nested classes ----")
      val c0 = Converters(classOf[Integer], classOf[Integer], classOf[String], null)
      out0.println(c0)         //finds toString
      out0.println(c0.get(fd0)(654))
      val c2 = Converters(classOf[Integer], classOf[String], classOf[Int], "parseInt")
      out0.println(c2)         //finds parseInt, a static method returning a scalar
      out0.println(c2.get(fd0)("12345"))
      val c1 = Converters(classOf[A.AA.X], classOf[Integer], classOf[String], null)
      out0.println(c1)         //finds cv
      out0.println(c1.get(fd0)(7896))
      val c3 = Converters(A.AA.Y.getClass, classOf[Integer], classOf[String], null)
      out0.println(c3)         //finds cv
      out0.println(c3.get(fd0)(5648))
      val c4 = Converters(classOf[A.AA.Z], classOf[Integer], classOf[String], null)
      out0.println(c4)         //finds cv
      out0.println(c4.get(fd0)(8931))
      val c5 = Converters(classOf[A1.AA.X], classOf[Integer], classOf[String], null)
      out0.println(c5)         //finds cv
      out0.println(c5.get(fd0)(78960))
      val c6 = Converters(A1.AA.Y.getClass, classOf[Integer], classOf[String], null)
      out0.println(c6)         //finds cv
      out0.println(c6.get(fd0)(56480))
      val c7 = Converters(classOf[A1.AA.Z], classOf[Integer], classOf[String], null)
      out0.println(c7)         //finds cv
      out0.println(c7.get(fd0)(89310))
      val c8 = Converters(A1.AA.Y.getClass, classOf[Integer], classOf[String], null)
      out0.println(c8)         //finds cv - repeat and see that there is no object spawning
      out0.println(c8.get(fd0)(564801))
      val c9 = Converters(classOf[java.util.Date], classOf[Long], classOf[java.util.Date], null)
      out0.println(c9)         //finds Date(long), a constructor ; also checks that we support primitive types
      out0.println(c9.get(fd0)(11111111L))
    }
  }
}
