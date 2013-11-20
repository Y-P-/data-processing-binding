package loader.reflect.test

import java.util.Date
import loader.reflect.Binder
import loader.reflect.StandardSolver
import loader.core.context.FieldAnnot
import loader.core.definition.Def
import loader.core.names.QName
import loader.reflect.AutoConvertData
import loader.reflect.DataActor
import utils.ByteArrayReader
import utils.LogTester._
import java.io.PrintWriter
import org.junit.Test
import scala.reflect.ClassTag
/*

object TestBinder {
    
  class U(val x:Int=0, val y:String=null, val z:Date=null, val a:Array[Int]=null, val b:Array[List[Array[Int]]]=null, val u:U=null, val v:Ex.Val, val w:MyEnum) {
    override def toString:String = s"U = ($x, $y, $z, [${if (a!=null) a.mkString(",") else ""}], $u, $v, $w)"
  }
  class V(x:Int=0,y:String=null,z:Date=null,a:Array[Int]=null,b:Array[List[Array[Int]]]=null,u:U=null,v:Ex.Val=null,w:MyEnum=null) extends U(x,y,z,a,b,u,v,w) {
    def toV(fd:FieldAnnot) = new V(x*3,y+s"-with toV and ${fd.param}",z,a,b,u,v,w)
    def toU = new U(x*3,y+"-with toU",z,a,b,u,v,w)
  }
  object V {
    def setInt(s:String,fd:ConvertData) = Integer.parseInt(s)*10
  }
  object W {
    def setInt(s:String,fd:ConvertData) = Integer.parseInt(s)*5
  }
  
  type F = (AnyRef,AnyRef)=>Unit
  implicit def forTest[E<:Def#Elt](b:Binder[E]):F = b(_).receive(_,null.asInstanceOf[E])
  
  def main(args:Array[String]):Unit = {
    import DataActor._

    val fdT = Map[Class[_],AutoConvertData]().withDefault(c=>fd("","","",""))
    val fdX = fd("[0-9]{2,}","","[01,46]","loader.reflect.test.TestBinder$W$.setInt")
    val fdY = Map[Class[_],AutoConvertData]().withDefault(c=>fd("","","",""))
    val fdZ = Map[Class[_],AutoConvertData](classOf[String]->fd("","dd-MM-yyyy","[01/01/2012 00:00,31/12/2013 23:59]",""))
    val fdU = Map[Class[_],AutoConvertData](classOf[V]->fd("","some param","","_.toU"))
    
    
    val o0 = new V
    val o1 = new V
    z0(o0,"10-12-2013")
  }
   
 */

   /** Tests:
     * A) Basic Conversions
     *   1) - check conversion to all default classes (tests basic converters)
     *   2) repeat but verify that regex works
     *   3) repeat but verify that valid works (borders) for these classes where applicable
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
object BinderTest {

  /** Test for basic conversions for Strings.
   *  Validates that all predefined converters work on default settings.
   */
  @Test class BinderBaseStringTest extends StandardTester {
    //no conversion data
    val f = Map[Class[_],AutoConvertData]().withDefault(c=>fd("","","",""))
    //basic binder: no collection, no conversion data, default String conversions
    implicit def forTest[E<:Def#Elt,U:ClassTag](fld:String):F = Binder(DataActor(implicitly[ClassTag[U]].runtimeClass)(fld).get,StandardSolver(),f,false)(_).receive(_,null.asInstanceOf[E])
    def apply(file:Solver,out0:PrintWriter) = {
      val u = new U
      def set(fld:String,v:String) = forTest[Def#Elt,U](fld).apply(u,v)
      set("pInt","1")
      set("pJInt","2")
      set("pLong","3")
      set("pJLong","4")
      set("pByte","5")
      set("pJByte","6")
      set("pShort","7")
      set("pJShort","8")
      set("pBoolean","t")
      set("pJBoolean","true")
      set("pChar","a")
      set("pJChar","b")
      set("pFloat","0.1")
      set("pJFloat","0.2")
      set("pDouble","0.3")
      set("pJDouble","0.4")
      set("pURL","file://./result")
      set("pURI","./result")
      set("pClass","loader.reflect.test.BinderTest$U")
      set("pDate","25/11/2012 00:00")
      set("pFile","./result")
      set("pEnum","ex1")
      set("pJEnum","c1")
      set("pString","some data")
      set("pCharA","some other data")
      set("pPat",".*")
      out0.println(u)
    }
  }
  /** Test for binders with no conversions.
   *  Validates that fields can be set by using the appropriate base data class.
   */
  @Test class BinderDirectTest extends StandardTester {
    //no conversion data
    val f = Map[Class[_],AutoConvertData]().withDefault(c=>fd("","","",""))
    //basic binder: no collection, no conversion data, no conversion
    implicit def forTest[E<:Def#Elt,U:ClassTag](fld:String):F = Binder(DataActor(implicitly[ClassTag[U]].runtimeClass)(fld).get,StandardSolver(),f,false)(_).receive(_,null.asInstanceOf[E])
    def apply(file:Solver,out0:PrintWriter) = {
      val u = new U
      def set(fld:String,v:Any) = forTest[Def#Elt,U](fld).apply(u,v)
      set("pInt",1)
      set("pJInt",2)
      set("pLong",3L)
      set("pJLong",4L)
      set("pByte",5.asInstanceOf[Byte])
      set("pJByte",6.asInstanceOf[Byte])
      set("pShort",7.asInstanceOf[Short])
      set("pJShort",8.asInstanceOf[Short])
      set("pBoolean",true)
      set("pJBoolean",true)
      set("pChar",'a')
      set("pJChar",'b')
      set("pFloat",0.1f)
      set("pJFloat",0.2f)
      set("pDouble",0.3)
      set("pJDouble",0.4)
      set("pURL",new java.net.URL("file://./result"))
      set("pURI",new java.net.URI("./result"))
      set("pClass",classOf[BinderTest.U])
      set("pDate",new Date("25/11/2012 00:00"))
      set("pFile",new java.io.File("./result"))
      set("pEnum",Ex.ex1)
      set("pJEnum",MyEnum.c1)
      set("pString","some data")
      set("pCharA","some other data".toCharArray)
      set("pPat",java.util.regex.Pattern.compile(".*"))
      out0.println(u)
    }
  }
  /** Test for binders with arrays, including arrays of arrays.
   *  Checks both primitive types and object type.
   */
  @Test class BinderArrayTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      val w = new W1(Array(),Array(),Array(),Array(),Array(),Array())
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      val a1 = f("a1"); a1("1");   a1("2");   a1("3");   a1.up()
      val b1 = f("b1"); b1("ex1"); b1("ex2"); b1("ex3"); b1.up()
      val a2 = f("a2"); a2.down(); a2("1"); a2("2"); a2("3"); a2.up()
                        a2.down(); a2("4"); a2("5"); a2("6"); a2.up(); a2.up()
      val b2 = f("b2"); b2.down(); b2("ex1"); b2("ex2"); b2("ex3"); b2.up()
                        b2.down(); b2("ex3"); b2("ex1"); b2("ex2"); b2.up()
                        b2.down(); b2("ex2"); b2("ex3"); b2("ex1"); b2.up(); b2.up()
      val a3 = f("a3"); a3.down(); a3.down(); a3("1"); a3("2"); a3("3"); a3.up()
                                   a3.down(); a3("4"); a3("5"); a3("6"); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("7"); a3("8"); a3.up()
                                   a3.down(); a3("9"); a3.up();
                                   a3.down(); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("9"); a3("10"); a3.up()
                                   a3.down(); a3("11"); a3.up(); a3.up(); a3.up()
      val b3 = f("b3"); b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3("ex3"); b3.up()
                                   b3.down(); b3("ex2"); b3("ex1"); b3("ex3"); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3.up()
                                   b3.down(); b3("ex1"); b3.up();
                                   b3.down(); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex2"); b3("ex1"); b3.up()
                                   b3.down(); b3("ex3"); b3.up(); b3.up(); b3.up()
      out0.println(w)
    }
  }
  /** Test for binders with a mix of arrays and collections.
   *  Checks both primitive types and object type.
   */
  @Test class BinderColTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      val w = new W2(List(),List(),Array(),Array(),Array(),Array())
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      val a1 = f("a1"); a1("1");   a1("2");   a1("3");   a1.up()
      val b1 = f("b1"); b1("ex1"); b1("ex2"); b1("ex3"); b1.up()
      val a2 = f("a2"); a2.down(); a2("1"); a2("2"); a2("3"); a2.up()
                        a2.down(); a2("4"); a2("5"); a2("6"); a2.up(); a2.up()
      val b2 = f("b2"); b2.down(); b2("ex1"); b2("ex2"); b2("ex3"); b2.up()
                        b2.down(); b2("ex3"); b2("ex1"); b2("ex2"); b2.up()
                        b2.down(); b2("ex2"); b2("ex3"); b2("ex1"); b2.up(); b2.up()
      val a3 = f("a3"); a3.down(); a3.down(); a3("1"); a3("2"); a3("3"); a3.up()
                                   a3.down(); a3("4"); a3("5"); a3("6"); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("7"); a3("8"); a3.up()
                                   a3.down(); a3("9"); a3.up();
                                   a3.down(); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("9"); a3("10"); a3.up()
                                   a3.down(); a3("11"); a3.up(); a3.up(); a3.up()
      val b3 = f("b3"); b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3("ex3"); b3.up()
                                   b3.down(); b3("ex2"); b3("ex1"); b3("ex3"); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3.up()
                                   b3.down(); b3("ex1"); b3.up();
                                   b3.down(); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex2"); b3("ex1"); b3.up()
                                   b3.down(); b3("ex3"); b3.up(); b3.up(); b3.up()
      out0.println(w)
    }
  }
  /** Test for binders with a more important mix of arrays and collections.
   *  Checks both primitive types and object type.
   */
  @Test class BinderMixedColTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      val w = new W3(scala.collection.immutable.HashSet(),scala.collection.immutable.HashSet(),List(),List(),Array(),Array())
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      val a1 = f("a1"); a1("1");   a1("2");   a1("3");  a1("3");  a1.up()
      val b1 = f("b1"); b1("ex1"); b1("ex2"); b1("ex3"); b1("ex1"); b1.up()
      val a2 = f("a2"); a2.down(); a2("1"); a2("2"); a2("3"); a2("1"); a2.up()
                        a2.down(); a2("4"); a2("5"); a2("6"); a2("4"); a2.up(); a2.up()
      val b2 = f("b2"); b2.down(); b2("ex1"); b2("ex2"); b2("ex3"); b2.up()
                        b2.down(); b2("ex3"); b2("ex1"); b2("ex2"); b2.up()
                        b2.down(); b2("ex2"); b2("ex3"); b2("ex1"); b2.up(); b2.up()
      val a3 = f("a3"); a3.down(); a3.down(); a3("1"); a3("2"); a3("3"); a3.up()
                                   a3.down(); a3("4"); a3("5"); a3("6"); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("7"); a3("8"); a3.up()
                                   a3.down(); a3("9"); a3("9"); a3.up();
                                   a3.down(); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("9"); a3("10"); a3.up()
                                   a3.down(); a3("11"); a3.up(); a3.up(); a3.up()
      val b3 = f("b3"); b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3("ex3"); b3.up()
                                   b3.down(); b3("ex2"); b3("ex1"); b3("ex3"); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3.up()
                                   b3.down(); b3("ex1"); b3.up();
                                   b3.down(); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex2"); b3("ex1"); b3.up()
                                   b3.down(); b3("ex3"); b3.up(); b3.up(); b3.up()
      out0.println(w)
    }
  }
  /** Test for binders with a more important mix of arrays and collections.
   *  Checks both primitive types and object type.
   *  Uses Java collections.
   */
  @Test class BinderJMixedColTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      val w = new W4(new java.util.HashSet(),new java.util.HashSet(),new java.util.LinkedList(),new java.util.LinkedList(),Array(),Array())
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      val a1 = f("a1"); a1("1");   a1("2");   a1("3");  a1("3");  a1.up()
      val b1 = f("b1"); b1("ex1"); b1("ex2"); b1("ex3"); b1("ex1"); b1.up()
      val a2 = f("a2"); a2.down(); a2("1"); a2("2"); a2("3"); a2("1"); a2.up()
                        a2.down(); a2("4"); a2("5"); a2("6"); a2("4"); a2.up(); a2.up()
      val b2 = f("b2"); b2.down(); b2("ex1"); b2("ex2"); b2("ex3"); b2.up()
                        b2.down(); b2("ex3"); b2("ex1"); b2("ex2"); b2.up()
                        b2.down(); b2("ex2"); b2("ex3"); b2("ex1"); b2.up(); b2.up()
      val a3 = f("a3"); a3.down(); a3.down(); a3("1"); a3("2"); a3("3"); a3.up()
                                   a3.down(); a3("4"); a3("5"); a3("6"); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("7"); a3("8"); a3.up()
                                   a3.down(); a3("9"); a3("9"); a3.up();
                                   a3.down(); a3.up(); a3.up()
                        a3.down(); a3.down(); a3("9"); a3("10"); a3.up()
                                   a3.down(); a3("11"); a3.up(); a3.up(); a3.up()
      val b3 = f("b3"); b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3("ex3"); b3.up()
                                   b3.down(); b3("ex2"); b3("ex1"); b3("ex3"); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex1"); b3("ex2"); b3.up()
                                   b3.down(); b3("ex1"); b3.up();
                                   b3.down(); b3.up(); b3.up()
                        b3.down(); b3.down(); b3("ex2"); b3("ex1"); b3.up()
                                   b3.down(); b3("ex3"); b3.up(); b3.up(); b3.up()
      out0.println(w)
    }
  }
  /** Test (elementary) for binders using Maps.
   */
  @Test class BinderMapTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      import loader.commons._
      val w = new V2()
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      val a1 = f("a1"); a1("1" --> "ex1"); a1("2" --> "ex2");   a1("3" --> "ex3");  a1.up()
      assert(w.a1(1)==Ex.ex1)
      assert(w.a1(2)==Ex.ex2)
      assert(w.a1(3)==Ex.ex3)
      val a2 = f("a2"); a2("1" --> ".*"); a2("2" --> "([a-z]+)*");   a2("3" --> "abc|abe");  a2.up()
      assert(w.a2(1).pattern==java.util.regex.Pattern.compile(".*").pattern)
      assert(w.a2(2).pattern==java.util.regex.Pattern.compile("([a-z]+)*").pattern)
      assert(w.a2(3).pattern==java.util.regex.Pattern.compile("abc|abe").pattern)
      val a3 = f("a3"); a3("ex1" --> "ex3"); a3("ex2" --> "ex1");   a3("ex3" --> "ex2");  a3.up()
      assert(w.a3(Ex.ex1)==Ex.ex3)
      assert(w.a3(Ex.ex2)==Ex.ex1)
      assert(w.a3(Ex.ex3)==Ex.ex2)
      val a4 = f("a4"); a4("2010" --> "01/01/2010 00:00"); a4("2011" --> "01/01/2011 00:00");   a4("2012" --> "01/01/2012 00:00");  a4.up()
      assert(w.a4(2010L)==new java.util.Date("01/01/2010 00:00"))
      assert(w.a4(2011L)==new java.util.Date("01/01/2011 00:00"))
      assert(w.a4(2012L)==new java.util.Date("01/01/2012 00:00"))
      val a5 = f("a5"); a5("x" --> "ex1"); a5("y" --> "ex2");   a5("z" --> "ex3");  a5.up()
      assert(w.a5("x")==Ex.ex1)
      assert(w.a5("y")==Ex.ex2)
      assert(w.a5("z")==Ex.ex3)
      out0.println(w)
    }
  }
  /** Test (elementary) for binders using Seq.
   */
  @Test class BinderSeqTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      val w = new V1()
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      def basicSTest(a:FHelper[_],ordered:Boolean=true):Unit = {
        a("ex1"); a("ex2"); a("ex3"); a.up()
        val fld = a.get().asInstanceOf[Traversable[Ex.Val]]
        assert(fld.size==3)
        if (ordered) {
          assert(fld.head==Ex.ex1)
          assert(fld.last==Ex.ex3)
          assert(fld.exists(_==Ex.ex2))
        } else {
          assert(fld.exists(_==Ex.ex1))
          assert(fld.exists(_==Ex.ex2))
          assert(fld.exists(_==Ex.ex3))
        }
      }
      def basicJTest(a:FHelper[_],ordered:Boolean=true):Unit = {
        a("ex1"); a("ex2"); a("ex3"); a.up()
        val fld = a.get().asInstanceOf[java.util.Collection[Ex.Val]]
        val r = fld.toArray
        assert(fld.size==3)
        if (ordered) {
          assert(r(0)==Ex.ex1)
          assert(r(2)==Ex.ex3)
          assert(r(1)==Ex.ex2)
        } else {
          assert(r.contains(Ex.ex1))
          assert(r.contains(Ex.ex2))
          assert(r.contains(Ex.ex3))
        }
      }
      val a1 = f("a1"); a1("1"); a1("2"); a1("3"); a1.up()
      assert(w.a1.contains(1))
      assert(w.a1.contains(2))
      assert(w.a1.contains(3))
      assert(!w.a1.contains(4))
      basicSTest(f("a2"),false)
      val a3 = f("a3"); a3("ex1"); a3("ex2"); a3.up()
      assert(w.a3.contains(Ex.ex1))
      assert(w.a3.contains(Ex.ex2))
      assert(!w.a3.contains(Ex.ex3))
      val a4 = f("a4"); a4("01/01/2010 00:00"); a4("01/01/2011 00:00"); a4("01/01/2012 00:00"); a4("01/01/2012 00:00"); a4.up()
      assert(w.a4.contains(new java.util.Date("01/01/2010 00:00")))
      assert(w.a4.contains(new java.util.Date("01/01/2011 00:00")))
      assert(w.a4.contains(new java.util.Date("01/01/2012 00:00")))
      assert(w.a4.size==3)
      val a5 = f("a5"); a5("ex1"); a5("ex2"); a5.up()
      assert(w.a5.front==Ex.ex1)
      assert(w.a5.length==2)
      basicSTest(f("a6"))
      val a7 = f("a7"); a7("ex2"); a7("ex2"); a7("ex1"); a7.up()
      assert(w.a7.exists(_==Ex.ex1))
      assert(!w.a7.exists(_==Ex.ex3))
      assert(w.a7.last==Ex.ex1)
      assert(w.a7.size==3)
      basicSTest(f("a8"),false)
      val a9 = f("a9"); a9("ex1"); a9("ex2"); a9("ex3"); a9("ex2"); a9.up()
      assert(w.a9.top==Ex.ex1)
      assert(w.a9.contains(Ex.ex3))
      assert(w.a9.size==4)
      basicSTest(f("a10"))
      basicSTest(f("a11"))
      basicSTest(f("a12"))
      basicSTest(f("a13"))
      basicSTest(f("a14"))
      basicJTest(f("a15"))
      basicJTest(f("a16"))
      val a17=f("a17"); a17("1"); a17("3"); a17("5"); a17.up()
      assert(w.a17.get(1))
      assert(w.a17.get(3))
      assert(w.a17.get(5))
      assert(!w.a17.get(0))
      assert(!w.a17.get(6))
      val a19=f("a19"); a19("a1"); a19("b1"); a19("c1"); a19.up()
      val l=new java.util.LinkedList[MyEnum](); l.add(MyEnum.a1); l.add(MyEnum.b1); l.add(MyEnum.c1)
      assert(w.a19.containsAll(l))
      basicJTest(f("a20"),false)
      basicJTest(f("a21"))
      basicJTest(f("a22"))
      basicJTest(f("a23"))
      basicJTest(f("a24"))
      basicJTest(f("a26"),false)
      basicJTest(f("a27"))
      out0.println(w)
    }
  }
  /** Test (elementary) for binders using Maps.
   */
  @Test class BinderBitsetTest extends StandardTester {
    def apply(file:Solver,out0:PrintWriter) = {
      val w = new WB(null)
      def f[X<:AnyRef:ClassTag](fld:String) = field(fld,w,fx)
      val a1 = f("a1"); a1("1"); a1("2");   a1("3");  a1.up()
      out0.println(w)
    }
  }
  class WB(val a1:scala.collection.immutable.BitSet) {
    override def toString = write(a1)
  }
  
  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  object Ex extends Enumeration {
    class Val protected[Ex] extends super.Val
    val ex1=new Val
    val ex2=new Val
    val ex3=new Val
  }
  class U(val pInt:Int,
          val pJInt:Integer,
          val pLong:Long,
          val pJLong:java.lang.Long,
          val pByte:Byte,
          val pJByte:java.lang.Byte,
          val pShort:Short,
          val pJShort:java.lang.Short,
          val pBoolean:Boolean,
          val pJBoolean:java.lang.Boolean,
          val pChar:Char,
          val pJChar:java.lang.Character,
          val pFloat:Float,
          val pJFloat:java.lang.Float,
          val pDouble:Double,
          val pJDouble:java.lang.Double,
          val pURL:java.net.URL,
          val pURI:java.net.URI,
          val pClass:Class[_],
          val pDate:java.util.Date,
          val pFile:java.io.File,
          val pEnum:Ex.Val,
          val pJEnum:MyEnum,
          val pString:String,
          val pCharA:Array[Char],
          val pPat:java.util.regex.Pattern
          ) {
    def this() = this(0,null,0,null,0,null,0,null,false,null,0,null,0,null,0,null,null,null,null,null,null,null,null,null,null,null)
    override def toString:String = s"$pInt\n$pJInt\n$pLong\n$pJLong\n$pByte\n$pJByte\n$pShort\n$pJShort\n$pBoolean\n$pJBoolean\n$pChar\n$pJChar\n$pFloat\n$pJFloat\n$pDouble\n$pJDouble\n$pURL\n$pURI\n$pClass\n$pDate\n$pFile\n$pEnum\n$pJEnum\n$pString\n${pCharA.mkString}\n$pPat"
  }
  def write[X](a:Traversable[X]) = if (a==null) "<null>" else scala.runtime.ScalaRunTime.stringOf(a)
  class W1(val a1:Array[Int],val b1:Array[Ex.Val],val a2:Array[Array[Int]],val b2:Array[Array[Ex.Val]],val a3:Array[Array[Array[Int]]],val b3:Array[Array[Array[Ex.Val]]]) {
    override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
  }
  class W2(val a1:List[Integer],val b1:List[Ex.Val],val a2:Array[List[Integer]],val b2:Array[List[Ex.Val]],val a3:Array[List[Array[Int]]],val b3:Array[List[Array[Ex.Val]]]) {
    override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
  }
  class W3(val a1:scala.collection.immutable.HashSet[Integer],val b1:scala.collection.immutable.HashSet[Ex.Val],val a2:List[scala.collection.immutable.HashSet[Integer]],val b2:List[scala.collection.immutable.HashSet[Ex.Val]],val a3:Array[List[scala.collection.immutable.HashSet[Integer]]],val b3:Array[List[scala.collection.immutable.HashSet[Ex.Val]]]) {
    override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
  }
  class W4(val a1:java.util.HashSet[Integer],val b1:java.util.HashSet[Ex.Val],val a2:java.util.LinkedList[java.util.HashSet[Integer]],val b2:java.util.LinkedList[java.util.HashSet[Ex.Val]],val a3:Array[java.util.LinkedList[java.util.HashSet[Integer]]],val b3:Array[java.util.LinkedList[java.util.HashSet[Ex.Val]]]) {
    import scala.collection.JavaConversions._ 
    override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
  }
  class V1(val a1:scala.collection.immutable.BitSet,
           val a2:scala.collection.immutable.HashSet[Ex.Val],
           val a3:scala.collection.immutable.List[Ex.Val],
           val a4:scala.collection.immutable.ListSet[java.util.Date],
           val a5:scala.collection.immutable.Queue[Ex.Val],
           val a6:scala.collection.immutable.IndexedSeq[Ex.Val],
           val a7:scala.collection.immutable.Iterable[Ex.Val],
           val a8:scala.collection.immutable.Set[Ex.Val],
           val a9:scala.collection.immutable.Stack[Ex.Val],
           val a10:scala.collection.immutable.LinearSeq[Ex.Val],
           val a11:scala.collection.immutable.Seq[Ex.Val],
           val a12:scala.collection.immutable.Stream[Ex.Val],
           val a13:scala.collection.immutable.Traversable[Ex.Val],
           val a14:scala.collection.immutable.Vector[Ex.Val],
           val a15:java.util.ArrayList[Ex.Val],
           val a16:java.util.ArrayDeque[Ex.Val],
           val a17:java.util.BitSet,
           val a19:java.util.EnumSet[MyEnum],
           val a20:java.util.HashSet[Ex.Val],
           val a21:java.util.LinkedHashSet[Ex.Val],
           val a22:java.util.LinkedList[Ex.Val],
           val a23:java.util.PriorityQueue[Ex.Val],
           val a24:java.util.Stack[Ex.Val],
           val a26:java.util.TreeSet[Ex.Val],
           val a27:java.util.Vector[Ex.Val]) {
    def this() = this(null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null)
    import collection.JavaConversions._
    override def toString = s"${write(a1)}\n${write(a2)}\n${write(a3)}\n${write(a4)}\n${write(a5)}\n${write(a6)}\n${write(a7)}\n${write(a8)}\n${write(a9)}\n${write(a10)}\n${write(a11)}\n${write(a12)}\n${write(a13)}\n${write(a14)}\n${write(a15)}\n${write(a16)}\n${a17}\n${write(a19)}\n${write(a20)}\n${write(a21)}\n${write(a22)}\n${write(a23)}\n${write(a24)}\n${write(a26)}\n${write(a27)}"
    //scala.collection.immutable.TreeSet[String]    fails because no default is possible (requires ordering which must be provided through an adapter)
    //scala.collection.immutable.SortedSet[String]  fails because no default is possible (requires ordering which must be provided through an adapter)
  }
  class V2(val a1:scala.collection.immutable.HashMap[Integer,Ex.Val],
           val a2:scala.collection.immutable.IntMap[java.util.regex.Pattern],
           val a3:scala.collection.immutable.ListMap[Ex.Val,Ex.Val],
           val a4:scala.collection.immutable.LongMap[java.util.Date],
           val a5:scala.collection.immutable.Map[String,Ex.Val]) {
    def this() = this(null,null,null,null,null)
    override def toString = s"${write(a1)}\n${write(a2)}\n${write(a3)}\n${write(a4)}\n${write(a5)}"
    //scala.collection.immutable.SortedMap[String,String]
    //scala.collection.immutable.TreeMap[String,String]
  }
  //java.util.EnumVal
  //java.util.HashMap
  //java.util.Hashtable
  //java.util.IdentityHashMap
  //java.util.LinkedHashMap
  //java.util.Properties
  //javajava.util.TreeMap
  //java.util.WeakHashMap
  

  def fd(check0:String,param0:String,valid0:String,convert0:String) = new AutoConvertData {
    def valid: String = valid0
    def check: String = check0
    def param: String = param0
    def convert: String = convert0
  }
  //no conversion data
  val fx = Map[Class[_],AutoConvertData]().withDefault(c=>fd("","","",""))
  class FHelper[X<:AnyRef:ClassTag](fld:String,x:X,fd:Map[Class[_],AutoConvertData]) {
    val cz  = implicitly[ClassTag[X]].runtimeClass
    var cur = Binder(DataActor(cz)(fld).getOrElse(throw new IllegalArgumentException(s"no field named $fld could be bound to $cz")),StandardSolver(),fd,true)(x)
    var r:Binder[Def#Elt]#Instance = _
    def down() = cur=cur.subInstance
    def apply(v:Any) = cur.receive(v,null)
    def up() = { cur.terminate(null); r=cur; cur=cur.container }
    def get() = r.read()
  }
  def field[X<:AnyRef:ClassTag](fld:String,x:X,fd:Map[Class[_],AutoConvertData]) = new FHelper(fld,x,fd)

  type F = (AnyRef,Any)=>Unit  
}

