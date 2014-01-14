package loader.reflect.test

import java.util.Date
import loader.reflect.Binder
import loader.reflect.StandardSolver
import loader.core.context.FieldAnnot
import loader.core.names.QName
import loader.reflect.AutoConvertData
import loader.reflect.DataActor
import utils.ByteArrayReader
import utils.LogTester._
import java.io.PrintWriter
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.reflect.ClassTag
import loader.reflect.ConvertData
import loader.reflect.Converters

/** Here we test converters as we need them.
 *  Scenario: at some point we have built an instance of U (likely spawned through reflexion)
 *  The expected data is V and we need a conversion:
 *  a) U contains a V builder with the required parameters (ConvertData,Def#Elt) => find and invoke
 *  b) the conversion is provided by an helper class W (could be V itself)
 *     - we don't want to have to clutter U with references to that class
 *     - this leaves the only possibilities that W is first rate (conversion = 'static' method)
 *       or that we can somehow provide an instance of W to work on.
 */
object ReflectiveConvertersTest {
  import loader.core.definition.Def
  import loader.core.CtxCore.{ Def => CtxDef }

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
    def toV1(u:S,fd:ConvertData,e:Def#Elt):V = invoke(u.out,1)
    def toV2(u:S,fd:ConvertData):V = invoke(u.out,2)
    def toV3(u:S,e:Def#Elt):V = invoke(u.out,3)
    def toV4(u:S):V = invoke(u.out,4)
  }
  
  //extensive check with derived classes
  class W(out:PrintWriter) extends V(out)
  object W {
    def invoke(out:PrintWriter, n:Int):W = { out.println(s"invoked V.toV$n"); new W(out) }
    def toV1(u:S,fd:ConvertData,e:Def#Elt):V = invoke(u.out,1)
    def toV2(u:S,fd:ConvertData):V = invoke(u.out,2)
    def toV3(u:S,e:CtxDef#Elt):W = invoke(u.out,3)
    def toV4(u:S):W = invoke(u.out,4)
    def toV5(u:T,fd:ConvertData,e:Def#Elt):V = invoke(u.out,5)
    def toV6(u:T,fd:ConvertData):V = invoke(u.out,6)
    def toV7(u:T,e:CtxDef#Elt):W = invoke(u.out,7)
    def toV8(u:T):W = invoke(u.out,8)
  }
  
  //basic object being converted from
  class S(val out:PrintWriter) {
    def invoke(n:Int) = { out.println(s"invoked U.toV$n"); new V(out) }
    def toV1(fd:ConvertData,e:Def#Elt):V = invoke(1)
    def toV2(fd:ConvertData):V = invoke(2)
    def toV3(e:Def#Elt):V = invoke(3)
    def toV4():V = invoke(4)
    def toV5(e:loader.core.CtxCore.Def#Elt,fd:ConvertData):V = invoke(5) //this also check that order is not important
    def toV6(e:Def#Elt,fd:ConvertData):V = invoke(6)
  }
  
  //derived object to convert from
  class T(out:PrintWriter) extends S(out)
  
  class X0(fd:ConvertData) {
    def invoke(out:PrintWriter, n:Int):W = { out.println(s"invoked V.toV$n"); new W(out) }
    def toV3(u:S,e:CtxDef#Elt):W = invoke(u.out,3)
    def toV4(u:T,e:Def#Elt):W    = invoke(u.out,4)
  }
  class X1(fd:ConvertData) extends X0(fd) {
    def toV1(u:S,e:Def#Elt):V    = invoke(u.out,1)
    def toV2(u:T,e:CtxDef#Elt):V = invoke(u.out,2)
  }

  /** Test to verify that DataActors are correctly found */
  @Test class ReflexiveConverterTest extends StandardTester {
    val cS = classOf[S]
    val cV = classOf[V]
    val cT = classOf[T]
    val cW = classOf[W]
    val fd0 = ConvertData.empty
    def apply(file:Solver,out0:PrintWriter) = {
      def checkBase[E<:Def#Elt:ClassTag](in:Class[_],cS:Class[_>:T<:S],cV:Class[_>:W<:V],fld:String):Unit = try {
        val x = Converters(in, cS, cV, fld)
        out0.println(x)
        if (x!=None) x.get(fd0)(new T(out0),null.asInstanceOf[E]) //we invoke with T just to be sure we have no exception. It doesn't matter for the test itself.
      } catch {
        case e:Exception=>out0.println(e.getMessage)
      }
      def check0[E<:Def#Elt:ClassTag](in:Class[_],cS:Class[_>:T<:S],cV:Class[_>:W<:V],n:Int):Unit = checkBase(in,cS,cV,s"toV$n")
      def check(in:Class[_],n:Int) = check0[Def#Elt](in,cS,cV,n)
      
      //checking that all converters are found in the source
      for (i <- 1 to 6) check(cS,i)  //5 must fail: CtxCore.Def#Elt cannot accept definition.Def#Elt
      //checking that all converters are found in the destination
      for (i <- 1 to 4) check(V.getClass,i)
      
      //checking sub-classes for Def#Elt
      out0.println(Converters(cS, cS, cV, "toV5"))  //None: CtxCore.Def#Elt cannot accept definition.Def#Elt
      out0.println(Converters(cS, cS, cV, "toV5")(ClassTag(classOf[loader.core.CtxCore.Def#Elt])))
      out0.println(Converters(cS, cS, cV, "toV1")(ClassTag(classOf[loader.core.CtxCore.Def#Elt])))
      
      //checking that all converters are found properly when using derived classes in various places
      out0.println ("---- S to V ----")
      for (i <- 1 to 8) check0[Def#Elt](W.getClass,classOf[S],classOf[V],i)  //1,2,4
      out0.println ("---- T to W ----")
      for (i <- 1 to 8) check0[Def#Elt](W.getClass,classOf[T],classOf[W],i)  //4,8
      out0.println ("---- S to W ----")
      for (i <- 1 to 8) check0[Def#Elt](W.getClass,classOf[S],classOf[W],i)  //4
      out0.println ("---- T to V ----")
      for (i <- 1 to 8) check0[Def#Elt](W.getClass,classOf[T],classOf[V],i)     //1,2,4,5,6,8
      out0.println ("---- S to V using CtxDef#Elt ----")
      for (i <- 1 to 8) check0[CtxDef#Elt](W.getClass,classOf[S],classOf[V],i)  //1,2,3,4
      out0.println ("---- T to W using CtxDef#Elt ----")
      for (i <- 1 to 8) check0[CtxDef#Elt](W.getClass,classOf[T],classOf[W],i)  //3,4,7,8
      out0.println ("---- S to W using CtxDef#Elt ----")
      for (i <- 1 to 8) check0[CtxDef#Elt](W.getClass,classOf[S],classOf[W],i)  //3,4
      out0.println ("---- T to V using CtxDef#Elt ----")
      for (i <- 1 to 8) check0[CtxDef#Elt](W.getClass,classOf[T],classOf[V],i)  //1,2,3,4,5,6,7,8
      
      //checking that we can find an appropriate converter when not giving a name
      //also used to check how the Reflect framework behaves given inheritance
      out0.println ("---- S to V using CtxDef#Elt in X1 instancied with fd0 ----")
      checkBase[CtxDef#Elt](classOf[X1], cS, cW, null) //OK, can find only V3
      checkBase[CtxDef#Elt](classOf[X1], cS, cV, null) //no, more than one: V1,V3
      checkBase[CtxDef#Elt](classOf[X1], cT, cV, null) //no, more than one: V1,V2,V3,V4
      checkBase[CtxDef#Elt](classOf[X1], cT, cW, null) //no, more than one: V3,V4
      out0.println ("---- S to V using CtxDef#Elt in X0 instancied with fd0 ----")
      checkBase[CtxDef#Elt](classOf[X0], cS, cW, null) //OK, can find only V3
      checkBase[CtxDef#Elt](classOf[X0], cS, cV, null) //OK, can find only V3
      checkBase[CtxDef#Elt](classOf[X0], cT, cV, null) //no, more than one: V3,V4
      checkBase[CtxDef#Elt](classOf[X0], cT, cW, null) //no, more than one: V3,V4
      out0.println ("---- S to V using Def#Elt in X1 instancied with fd0 ----")
      checkBase[Def#Elt](classOf[X1], cS, cW, null) //no
      checkBase[Def#Elt](classOf[X1], cS, cV, null) //OK, can find only V1
      checkBase[Def#Elt](classOf[X1], cT, cV, null) //no, more than one: V1,V4
      checkBase[Def#Elt](classOf[X1], cT, cW, null) //OK, can find only V4
      out0.println ("---- S to V using Def#Elt in X0 instancied with fd0 ----")
      checkBase[Def#Elt](classOf[X0], cS, cW, null) //no
      checkBase[Def#Elt](classOf[X0], cS, cV, null) //no
      checkBase[Def#Elt](classOf[X0], cT, cV, null) //OK, can find only V4
      checkBase[Def#Elt](classOf[X0], cT, cW, null) //OK, can find only V4
      
      out0.println ("---- Standard Java classes, compatibility with static methods, classes without ConvertData, deep nested classes ----")
      val c0 = Converters(classOf[Integer], classOf[Integer], classOf[String], null)
      out0.println(c0)         //finds toString
      out0.println(c0.get(fd0)(654,null))
      val c2 = Converters(classOf[Integer], classOf[String], classOf[Int], "parseInt")
      out0.println(c2)         //finds parseInt, a static method returning a scalar
      out0.println(c2.get(fd0)("12345",null))
      val c1 = Converters(classOf[A.AA.X], classOf[Integer], classOf[String], null)
      out0.println(c1)         //finds cv
      out0.println(c1.get(fd0)(7896,null))
      val c3 = Converters(A.AA.Y.getClass, classOf[Integer], classOf[String], null)
      out0.println(c3)         //finds cv
      out0.println(c3.get(fd0)(5648,null))
      val c4 = Converters(classOf[A.AA.Z], classOf[Integer], classOf[String], null)
      out0.println(c4)         //finds cv
      out0.println(c4.get(fd0)(8931,null))
      val c5 = Converters(classOf[A1.AA.X], classOf[Integer], classOf[String], null)
      out0.println(c5)         //finds cv
      out0.println(c5.get(fd0)(78960,null))
      val c6 = Converters(A1.AA.Y.getClass, classOf[Integer], classOf[String], null)
      out0.println(c6)         //finds cv
      out0.println(c6.get(fd0)(56480,null))
      val c7 = Converters(classOf[A1.AA.Z], classOf[Integer], classOf[String], null)
      out0.println(c7)         //finds cv
      out0.println(c7.get(fd0)(89310,null))
      val c8 = Converters(A1.AA.Y.getClass, classOf[Integer], classOf[String], null)
      out0.println(c8)         //finds cv - repeat and see that there is no object spawning
      out0.println(c8.get(fd0)(564801,null))
    }
  }
}
