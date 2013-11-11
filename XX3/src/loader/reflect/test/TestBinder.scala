package loader.reflect.test

import java.util.Date
import loader.reflect.Binder
import loader.reflect.StandardSolver
import loader.core.context.FieldAnnot
import loader.core.definition.Def
import loader.core.names.QName
import loader.reflect.AutoConvertData
import loader.reflect.ConvertData
import loader.reflect.DataActor

object TestBinder {
  
  class U(val x:Int=0, val y:String=null, val z:Date=null, val a:Array[Int]=null, val b:Array[List[Array[Int]]]=null, val u:U=null) {
    override def toString:String = s"U = ($x, $y, $z, [${if (a!=null) a.mkString(",") else ""}], $u)"
  }
  class V(x:Int=0,y:String=null,z:Date=null,a:Array[Int]=null,b:Array[List[Array[Int]]]=null,u:U=null) extends U(x,y,z,a,b,u) {
    def toV(fd:FieldAnnot) = new V(x*3,y+s"-with toV and ${fd.param}",z,a,null)
    def toU = new U(x*3,y+"-with toU",z,a,null)
  }
  object V {
    def setInt(s:String,fd:ConvertData) = Integer.parseInt(s)*10
  }
  object W {
    def setInt(s:String,fd:ConvertData) = Integer.parseInt(s)*5
  }
  
  def fd(check0:String,param0:String,valid0:String,convert0:String,isCol:Boolean=false) = new AutoConvertData {
    def valid: String = valid0   
    def check: String = check0
    def param: String = param0
    def convert: String = convert0
  }
  
  type F = (AnyRef,AnyRef)=>Unit
  implicit def forTest[E<:Def#Elt](b:Binder[E]):F = b(_).receive(_,null.asInstanceOf[E])
  
  def main(args:Array[String]):Unit = {    
    import DataActor._
    /* Tests:
     * A) Basic Conversions
     *   1) check conversion to all default classes (tests basic converters)
     *   2) repeat but verify that regex works
     *   3) repeat but verify that valid works (borders) for these classes with valid
     * B) Method Conversions
     *   1) check conversion with a local method from source
     *     a) with no param
     *     b) with fd param
     *   2) check conversion with a method from an object
     *     a) with source param
     *     b) with source and fd param
     *   3) check conversion with a method from a class with default constructor
     *     a) with source param
     *     b) with source and fd param
     * C) Named Conversions
     *   1) Check that named conversions are found and that the class narrowing works
     * D) Derived classes
     *   1) check that a derived class can be used natively (nothing specific to declare to fill it)
     *   2) check that a derived class can be used where the parent class is expected
     *   3) check that a derived class can be used where it is itself expected
     *   4) check that a parent class is rejected where a child class is expected
     *   5) check that the class narrowing works
     */
    val fdX = fd("[0-9]{2,}","","[01,46]","loader.reflect.test.TestBinder$W$.setInt")
    val fdY = Map[Class[_],AutoConvertData](classOf[String]->fd("","","",""))
    val fdZ = Map[Class[_],AutoConvertData](classOf[String]->fd("","dd-MM-yyyy","[01/01/2012 00:00,31/12/2013 23:59]",""))
    val fdU = Map[Class[_],AutoConvertData](classOf[V]->fd("","some param","","_.toU"))
    val x0:F = Binder(classOf[U]("x").get,StandardSolver(),fdX,false)
    val y0:F = Binder(classOf[U]("y").get,StandardSolver(),fdY,false)
    val z0:F = Binder(classOf[U]("z").get,StandardSolver(),fdZ,false)
    val a0   = Binder(classOf[U]("a").get,StandardSolver(),fdY,true)
    val b0   = Binder(classOf[U]("b").get,StandardSolver(),fdY,true)
    val u0:F = Binder(classOf[U]("u").get,StandardSolver(),fdU,false)
    
    
    val o0 = new V
    val o1 = new V
    x0(o0,"34")
    y0(o0,"abc")
    z0(o0,"10-12-2013")
    val a = a0(o0)
    a.receive("1",null)
    a.receive("2",null)
    a.terminate()
    println(a.read)
    val b = b0(o0)
    val x1 = b.subInstance
    val xx1 = x1.subInstance
    xx1.receive("1",null)
    xx1.receive("2",null)
    xx1.terminate()
    val xx2 = x1.subInstance
    xx2.receive("3",null)
    xx2.receive("4",null)
    xx2.receive(new Integer(45),null)
    xx2.terminate()
    x1.terminate
    val y1 = b.subInstance
    val yy1 = y1.subInstance
    yy1.receive("5",null)
    yy1.receive("6",null)
    yy1.terminate()
    val yy2 = y1.subInstance
    yy2.receive("7",null)
    yy2.receive("8",null)
    yy2.terminate()
    y1.terminate
    b.terminate()
    
    println(o0)
    
    x0(o1,"12")
    u0(o1,o0)
    println(o1)
    
  }

}