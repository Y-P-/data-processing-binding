package utils.tests.reflect

import scala.reflect.ClassTag
import utils.reflect._

abstract protected class Helper {
    
  ////////////////////////////////////////////////////////////////////////////////////////////////////
  // Utilities for a better life
  ////////////////////////////////////////////////////////////////////////////////////////////////////

  def fd(check0:String,param0:String,valid0:String,convert0:String) = new AutoConvertData {
    def valid: String = valid0
    def check: String = check0
    def param: String = param0
    def convert: String = convert0
  }
  //no conversion data
  val fx = Map[Class[_],AutoConvertData]().withDefault(c=>fd("","","",""))
  /** A simple utility that fetches the Binder on the DataActor fld in object x */
  def get[X<:AnyRef:ClassTag](fld:String,x:X) = {
    val cz = implicitly[ClassTag[X]].runtimeClass
    Binder(DataActor(cz,fld,"f").getOrElse(throw new IllegalArgumentException(s"no field named $fld could be bound to $cz")),new ConversionSolver(),fx(cz),true)(x)
  }
  def write[X](a:Traversable[X]) = if (a==null) "<null>" else scala.runtime.ScalaRunTime.stringOf(a)
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Raw structures that can be used for the tests
////////////////////////////////////////////////////////////////////////////////////////////////////
  
object Ex extends Enumeration {
  class Val protected[Ex] extends super.Val
  val ex1=new Val
  val ex2=new Val
  val ex3=new Val
}
  
class Histo(name:String) extends scala.collection.mutable.Undoable {
  def undo():Unit = ()
  override def toString = name
}
