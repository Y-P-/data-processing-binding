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
object ConvertersTest {
  
  import loader.core.definition.Def
  class U(val out:PrintWriter) {
    def invoke(n:Int) = { out.println(s"invoked U.toV$n"); new V(out) }
    def toV1(fd:ConvertData,e:Def#Elt):V = invoke(1)
    def toV2(fd:ConvertData):V = invoke(2)
    def toV3(e:Def#Elt):V = invoke(3)
    def toV4():V = invoke(4)
    def toV5(e:loader.core.CtxCore.Def#Elt,fd:ConvertData):V = invoke(5) //this also check that order is not important
    def toV6(e:Def#Elt,fd:ConvertData):V = invoke(6)
  }

  /** Test to verify that DataActors are correctly found */
  @Test class ReflexiveConverterTest extends StandardTester {
    val cU = classOf[U]
    val cV = classOf[V]
    val fd0 = ConvertData.empty
    def apply(file:Solver,out0:PrintWriter) = {
      def check(in:Class[_],n:Int) = {
        val x = Converters(in, cU, cV, s"toV$n")
        out0.println(x)
        if (x!=None) x.get(fd0)(new U(out0),null)      
      }
      //checking that all converters are found in the source
      for (i <- 1 to 6) check(cU,i)
      //checking that all converters are found in the destination
      for (i <- 1 to 4) check(cV,1)
      
      //checking sub-classes for Def#Elt
      println(Converters(cU, cU,  cV, "toV5"))  //None: CtxCore.Def#Elt cannot accept definition.Def#Elt
      println(Converters(cU, cU,  cV, "toV5")(ClassTag(classOf[loader.core.CtxCore.Def#Elt])))
      println(Converters(cU, cU,  cV, "toV1")(ClassTag(classOf[loader.core.CtxCore.Def#Elt])))
    }
  }
}

class V(out:PrintWriter) {
}
object V {
  import ConvertersTest.U
  import loader.core.definition.Def
  def invoke(out:PrintWriter, n:Int) = { out.println(s"invoked V.toV$n"); new V(out) }
  def toV1(u:U,fd:ConvertData,e:Def#Elt):V = invoke(u.out,1)
  def toV2(u:U,fd:ConvertData):V = invoke(u.out,2)
  def toV3(u:U,e:Def#Elt):V = invoke(u.out,3)
  def toV4(u:U):V = invoke(u.out,4)    
}
