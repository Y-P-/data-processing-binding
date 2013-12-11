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
import loader.reflect.Converter
import loader.reflect.Converters

object ConvertersTest {

  /** Test to verify that DataActors are correctly found */
  @Test class ReflexiveConverterTest extends StandardTester {
    //import loader.core.definition.Def
    import loader.core.CtxCore.Def //using a subclass for Elt is more stressful
    class U {
      def toV1(fd:ConvertData,e:Def#Elt):V = new V
      def toV2(fd:ConvertData):V = new V
      def toV3(e:Def#Elt):V = new V
      def toV4():V = new V
    }
    class V {
      def toV1(u:U,fd:ConvertData,e:Def#Elt):V = new V
      def toV2(u:U,fd:ConvertData):V = new V
      def toV3(u:U,e:Def#Elt):V = new V
      def toV4(u:U):V = new V
    }
    class W {
    }
    def apply(file:Solver,out0:PrintWriter) = {
      //import out0.println  //write to out0 (remove to write to console)
      //checking that all converters are found in the source
      println(Converters(classOf[Def#Elt], classOf[U], classOf[U],  classOf[V], "toV1"))
      println(Converters(classOf[Def#Elt], classOf[U], classOf[U],  classOf[V], "toV2"))
      println(Converters(classOf[Def#Elt], classOf[U], classOf[U],  classOf[V], "toV3"))
      println(Converters(classOf[Def#Elt], classOf[U], classOf[U],  classOf[V], "toV4"))
      //checking that all converters are found in the destination
      println(Converters(classOf[Def#Elt], classOf[V], classOf[U],  classOf[V], "toV1"))
      println(Converters(classOf[Def#Elt], classOf[V], classOf[U],  classOf[V], "toV2"))
      println(Converters(classOf[Def#Elt], classOf[V], classOf[U],  classOf[V], "toV3"))
      println(Converters(classOf[Def#Elt], classOf[V], classOf[U],  classOf[V], "toV4"))
    }
  }
}