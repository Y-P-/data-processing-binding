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
     *   1) - check conversion to all default classes (tests basic converters) OK
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
//@RunWith(classOf[JUnit4])
object CollectionBinderTest extends Helper {
  import Binder._
  
  /** Test for binders with arrays, including arrays of arrays.
   *  Checks both primitive types and object type.
   */
  @Test class BinderArrayTest extends StandardTester {
    class W(val a1:Array[Int],val b1:Array[Ex.Val],val a2:Array[Array[Int]],val b2:Array[Array[Ex.Val]],val a3:Array[Array[Array[Int]]],val b3:Array[Array[Array[Ex.Val]]]) {
      override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
    }
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      val w = new W(Array(),Array(),Array(),Array(),Array(),Array())
      get("a1",w).u(o("1","2","3"))
      get("b1",w).u(o("ex1","ex2","ex3"))
      get("a2",w).u(u(o("1","2","3")), u(o("4","5","6")))
      get("b2",w).u(u(o("ex1","ex2","ex3")),u(o("ex3","ex1","ex2")),u(o("ex2","ex3","ex1")))
      get("a3",w).u(u(u(o("1","2","3")),u(o("4","5","6"))), u(u(o("7","8")),u(o("9")),u(o())), u(u(o("9","10")),u(o("11"))))
      get("b3",w).u(u(u(o("ex1","ex2","ex3")),u(o("ex2","ex1","ex3"))), u(u(o("ex1","ex2")),u(o("ex1")),u(o())), u(u(o("ex2","ex1")),u(o("ex3"))))
      out0.println(w)
    }
  }
  /** Test for binders with a mix of arrays and collections.
   *  Checks both primitive types and object type.
   */
  @Test class BinderColTest extends StandardTester {
    class W(val a1:List[Integer],val b1:List[Ex.Val],val a2:Array[List[Integer]],val b2:Array[List[Ex.Val]],val a3:Array[List[Array[Integer]]],val b3:Array[List[Array[Ex.Val]]]) {
      override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
    }
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      val w = new W(List(),List(),Array(),Array(),Array(),Array())
      get("a1",w).u(o("1","2","3"))
      get("b1",w).u(o("ex1","ex2","ex3"))
      get("a2",w).u(u(o("1","2","3")), u(o("4","5","6")))
      get("b2",w).u(u(o("ex1","ex2","ex3")),u(o("ex3","ex1","ex2")),u(o("ex2","ex3","ex1")))
      get("a3",w).u(u(u(o("1","2","3")),u(o("4","5","6"))), u(u(o("7","8")),u(o("9")),u(o())), u(u(o("9","10")),u(o("11"))))
      get("b3",w).u(u(u(o("ex1","ex2","ex3")),u(o("ex2","ex1","ex3"))), u(u(o("ex1","ex2")),u(o("ex1")),u(o())), u(u(o("ex2","ex1")),u(o("ex3"))))
      out0.println(w)
    }
  }
  /** Test for binders with a more important mix of arrays and collections.
   *  Checks both primitive types and object type.
   *  Note: we introduce repetitions for Set tests.
   */
  @Test class BinderMixedColTest extends StandardTester {
    class W(val a1:scala.collection.immutable.HashSet[Integer],val b1:scala.collection.immutable.HashSet[Ex.Val],val a2:List[scala.collection.immutable.HashSet[Integer]],val b2:List[scala.collection.immutable.HashSet[Ex.Val]],val a3:Array[List[scala.collection.immutable.HashSet[Integer]]],val b3:Array[List[scala.collection.immutable.HashSet[Ex.Val]]]) {
      override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
    }
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      val w = new W(scala.collection.immutable.HashSet(),scala.collection.immutable.HashSet(),List(),List(),Array(),Array())
      get("a1",w).u(o("1","2","3","3"))
      get("b1",w).u(o("ex1","ex2","ex3","ex1"))
      get("a2",w).u(u(o("1","2","3","1")), u(o("4","5","6","4")))
      get("b2",w).u(u(o("ex1","ex2","ex3")),u(o("ex3","ex1","ex2")),u(o("ex2","ex3","ex1")))
      get("a3",w).u(u(u(o("1","2","3")),u(o("4","5","6"))), u(u(o("7","8")),u(o("9","9")),u(o())), u(u(o("9","10")),u(o("11"))))
      get("b3",w).u(u(u(o("ex1","ex2","ex3")),u(o("ex2","ex1","ex3"))), u(u(o("ex1","ex2")),u(o("ex1")),u(o())), u(u(o("ex2","ex1")),u(o("ex3"))))
      out0.println(w)
    }
  }
  /** Test for binders with a more important mix of arrays and collections.
   *  Checks both primitive types and object type.
   *  Uses Java collections.
   */
  @Test class BinderJMixedColTest extends StandardTester {
    class W4(val a1:java.util.HashSet[Integer],val b1:java.util.HashSet[Ex.Val],val a2:java.util.LinkedList[java.util.HashSet[Integer]],val b2:java.util.LinkedList[java.util.HashSet[Ex.Val]],val a3:Array[java.util.LinkedList[java.util.HashSet[Integer]]],val b3:Array[java.util.LinkedList[java.util.HashSet[Ex.Val]]]) {
      import scala.collection.JavaConversions._ 
      override def toString = s"${write(a1)}\n${write(b1)}\n${write(a2)}\n${write(b2)}\n${write(a3)}\n${write(b3)}\n"
    }
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      val w = new W4(new java.util.HashSet(),new java.util.HashSet(),new java.util.LinkedList(),new java.util.LinkedList(),Array(),Array())
      get("a1",w).u(o("1","2","3","3"))
      get("b1",w).u(o("ex1","ex2","ex3","ex1"))
      get("a2",w).u(u(o("1","2","3","1")), u(o("4","5","6","4")))
      get("b2",w).u(u(o("ex1","ex2","ex3")),u(o("ex3","ex1","ex2")),u(o("ex2","ex3","ex1")))
      get("a3",w).u(u(u(o("1","2","3")),u(o("4","5","6"))), u(u(o("7","8")),u(o("9","9")),u(o())), u(u(o("9","10")),u(o("11"))))
      get("b3",w).u(u(u(o("ex1","ex2","ex3")),u(o("ex2","ex1","ex3"))), u(u(o("ex1","ex2")),u(o("ex1")),u(o())), u(u(o("ex2","ex1")),u(o("ex3"))))
      out0.println(w)
    }
  }
  /** Test (elementary) for binders using Maps.
   */
  @Test class BinderMapTest extends StandardTester {
    //Structure for testing which maps we can spawn
    class V2(val a1:scala.collection.immutable.HashMap[Ex.Val,Ex.Val],
           val a2:scala.collection.immutable.IntMap[java.util.regex.Pattern],
           val a3:scala.collection.immutable.ListMap[Ex.Val,Ex.Val],
           val a4:scala.collection.immutable.LongMap[java.util.Date],
           val a5:scala.collection.immutable.Map[Ex.Val,Ex.Val],
           val a6:scala.collection.mutable.HashMap[Ex.Val,Ex.Val],
           val a9:scala.collection.mutable.LinkedHashMap[Ex.Val,Ex.Val],
           val a10:scala.collection.mutable.ListMap[Ex.Val,Ex.Val],
           val a11:scala.collection.mutable.Map[Ex.Val,Ex.Val],
           val a13:scala.collection.mutable.WeakHashMap[Ex.Val,Ex.Val],
           val a14:java.util.EnumMap[MyEnum,Ex.Val],
           val a15:java.util.HashMap[Ex.Val,Ex.Val],
           val a16:java.util.IdentityHashMap[Ex.Val,Ex.Val],
           val a17:java.util.LinkedHashMap[Ex.Val,Ex.Val],
           val a18:java.util.Properties,
           val a20:java.util.TreeMap[Ex.Val,Ex.Val],
           val a21:java.util.WeakHashMap[Ex.Val,Ex.Val]) {
      def this() = this(null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null)
      import collection.JavaConversions._
      //NOTE: we don't write a16 because IdentityHashMap explicitly states that the order can change
      override def toString = s"a1 -> ${write(a1)}\na2 -> ${write(a2)}\na3 -> ${write(a3)}\na4 -> ${write(a4)}\na5 -> ${write(a5)}\na6 -> ${write(a6)}\na9 -> ${write(a9)}\na10 -> ${write(a10)}\na11 -> ${write(a11)}\na13 -> ${write(a13)}\na14 -> ${write(a14)}\na15 -> ${write(a15)}\na17 -> ${write(a17)}\na18 -> ${write(a18)}\na20 -> ${write(a20)}\na21 -> ${write(a21)}"
      //scala.collection.immutable.SortedMap[String,String]
      //scala.collection.immutable.TreeMap[String,String]
      //scala.collection.mutable.HashTable[Ex.Val,Ex.Val]    fails because it is not Traversable (not a real collection actually)
    }
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      import loader.commons._
      val w = new V2()
      //builds a 'map' ex3->ex1,ex1->ex2,ex2->ex3 => checks that the collection works and that the correct converters are called (scala collections)
      def basicSTest(nom:String,ordered:Boolean):Unit = {
        val x = get(nom,w)
        x.u(o("ex3" --> "ex1","ex1" --> "ex2","ex2" --> "ex3"))
        val fld = x.read().asInstanceOf[scala.collection.Map[Ex.Val,Ex.Val]]
        assert(fld.size==3)
        if (ordered) {
          val i=fld.keys.iterator
          val a = Array(Ex.ex3,Ex.ex1,Ex.ex2)
          for (n <- 0 until fld.size) assert(a(n)==i.next)
        }
        assert(fld(Ex.ex3)==Ex.ex1)
        assert(fld(Ex.ex1)==Ex.ex2)
        assert(fld(Ex.ex2)==Ex.ex3)          
      }
      //builds a 'map' ex3->ex1,ex1->ex2,ex2->ex3 => checks that the collection works and that the correct converters are called (java collections)
      def basicJTest(nom:String,ordered:Boolean):Unit = {
        val x = get(nom,w)
        x.u(o("ex3" --> "ex1","ex1" --> "ex2","ex2" --> "ex3"))
        val fld = x.read().asInstanceOf[java.util.Map[Ex.Val,Ex.Val]]
        assert(fld.size==3)
        if (ordered) {
          val i=fld.values.iterator
          val a = Array(Ex.ex1,Ex.ex2,Ex.ex3)
          for (n <- 0 until fld.size) assert(a(n)==i.next)
        }
        assert(fld.get(Ex.ex3)==Ex.ex1)
        assert(fld.get(Ex.ex1)==Ex.ex2)
        assert(fld.get(Ex.ex2)==Ex.ex3)          
      }
      //runs basicSTest for fields aNN where NN loops in the list (scala)
      def suiteS(ordered:Boolean,x:Int*):Unit = for (i<-x) basicSTest(s"a$i",ordered)
      //runs basicSTest for fields aNN where NN loops in the list (scala)
      def suiteJ(ordered:Boolean,x:Int*):Unit = for (i<-x) basicJTest(s"a$i",ordered)
      
      suiteS(false,1,3,5,6,10,11,13)
      suiteS(true,9)
      suiteJ(false,15,16,17,20,21)
      
      get("a2",w).u(o("1" --> ".*","2" --> "([a-z]+)*","3" --> "abc|abe"))
      assert(w.a2(1).pattern==java.util.regex.Pattern.compile(".*").pattern)
      assert(w.a2(2).pattern==java.util.regex.Pattern.compile("([a-z]+)*").pattern)
      assert(w.a2(3).pattern==java.util.regex.Pattern.compile("abc|abe").pattern)
      get("a4",w).u(o("2010" --> "01/01/2010 00:00","2011" --> "01/01/2011 00:00","2012" --> "01/01/2012 00:00"))
      assert(w.a4(2010L)==new java.util.Date("01/01/2010 00:00"))
      assert(w.a4(2011L)==new java.util.Date("01/01/2011 00:00"))
      assert(w.a4(2012L)==new java.util.Date("01/01/2012 00:00"))
      get("a14",w).u(o("a1" --> "ex1","b1" --> "ex2","c1" --> "ex3"))
      assert(w.a14.get(MyEnum.a1)==Ex.ex1)
      assert(w.a14.get(MyEnum.b1)==Ex.ex2)
      assert(w.a14.get(MyEnum.c1)==Ex.ex3)
      get("a18",w).u(o("p1" --> "tex1","p2" --> "tex2","p3" --> "tex3"))
      assert(w.a18.get("p1")=="tex1")
      assert(w.a18.get("p2")=="tex2")
      assert(w.a18.get("p3")=="tex3")
      
      out0.println(w)
    }
  }
  /** Test (elementary) for binders using Seq.
   */
  @Test class BinderSeqTest extends StandardTester {
    //Structure for testing which iterable we can spawn
    class V(val a1:scala.collection.immutable.BitSet,
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
           val a27:java.util.Vector[Ex.Val],
           val a28:scala.collection.mutable.ArrayBuffer[Ex.Val],
           val a30:scala.collection.mutable.ArraySeq[Ex.Val],
           val a31:scala.collection.mutable.ArrayStack[Ex.Val],
           val a32:scala.collection.mutable.BitSet,
           val a33:scala.collection.mutable.Buffer[Ex.Val],
           val a34:scala.collection.mutable.DoubleLinkedList[Ex.Val],
           val a35:scala.collection.mutable.HashSet[Ex.Val],
           val a36:scala.collection.mutable.IndexedSeq[Ex.Val],
           val a37:scala.collection.mutable.Iterable[Ex.Val],
           val a38:scala.collection.mutable.LinearSeq[Ex.Val],
           val a39:scala.collection.mutable.LinkedHashSet[Ex.Val],
           val a40:scala.collection.mutable.LinkedList[Ex.Val],
           val a41:scala.collection.mutable.ListBuffer[Ex.Val],
           val a42:scala.collection.mutable.MutableList[Ex.Val],
           val a44:scala.collection.mutable.Queue[Ex.Val],
           val a45:scala.collection.mutable.ResizableArray[Ex.Val],
           val a46:scala.collection.mutable.Seq[Ex.Val],
           val a47:scala.collection.mutable.Set[Ex.Val],
           val a48:scala.collection.mutable.RevertibleHistory[Histo,Ex.Val],
           val a49:scala.collection.mutable.Stack[Ex.Val],
           val a50:scala.collection.mutable.Traversable[Ex.Val],
           val a51:scala.collection.mutable.History[Ex.Val,Ex.Val],
           val a52:scala.collection.mutable.UnrolledBuffer[Ex.Val]) {
      def this() = this(null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null)
      import collection.JavaConversions._
      override def toString = s"a1 -> ${write(a1)}\na2 -> ${write(a2)}\na3 -> ${write(a3)}\na6 -> ${write(a4)}\na5 -> ${write(a5)}\na6 -> ${write(a6)}\na7 -> ${write(a7)}\na8 -> ${write(a8)}\na9 -> ${write(a9)}\na10 -> ${write(a10)}\na11 -> ${write(a11)}\na12 -> ${write(a12)}\na13 -> ${write(a13)}\na14 -> ${write(a14)}\na15 -> ${write(a15)}\na16 -> ${write(a16)}\na17 -> ${a17}\na19 -> ${write(a19)}\na20 -> ${write(a20)}\na21 -> ${write(a21)}\na22 -> ${write(a22)}\na23 -> ${write(a23)}\na24 -> ${write(a24)}\na26 -> ${write(a26)}\na27 -> ${write(a27)}\na28 -> ${write(a28)}\na30 -> ${write(a30)}\na31 -> ${write(a31)}\na32 -> ${write(a32)}\na33 -> ${write(a33)}\na34 -> ${write(a34)}\na35 -> ${write(a35)}\na36 -> ${write(a36)}\na37 -> ${write(a37)}\na38 -> ${write(a38)}\na39 -> ${write(a39)}\na40 -> ${write(a40)}\na41 -> ${write(a41)}\na42 -> ${write(a42)}\na44 -> ${write(a44)}\na45 -> ${write(a45)}\na46 -> ${write(a46)}\na47 -> ${write(a47)}\na48 -> ${write(a48)}\na49 -> ${write(a49)}\na50 -> ${write(a50)}\na51 -> ${write(a51)}\na52 -> ${write(a52)}"
      //scala.collection.immutable.TreeSet[String]     fails because no default is possible (requires ordering which must be provided through an adapter)
      //scala.collection.immutable.SortedSet[String]   fails because no default is possible (requires ordering which must be provided through an adapter)
      //scala.collection.mutable.ArrayBuilder[Ex.Val]  fails because it is not Traversable
      //scala.collection.mutable.TreeSet[Ex.Val]       see above
      //scala.collection.mutable.SortedSet[Ex.Val]     see above
      //scala.collection.mutable.PriorityQueue[Ex.Val] fails because no default is possible (requires ordering which must be provided through an adapter)
    }
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      val w = new V()
      //builds a 'sequence' ex1,ex2,ex3 => checks that the collection works and that the correct converter is called (scala collections)
      def basicSTest(nom:String,ordered:Boolean=true):Unit = {
        val x = get(nom,w)
        x.u(o("ex1","ex2","ex3"))
        val fld = x.read().asInstanceOf[Traversable[Ex.Val]]
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
      //builds a 'sequence' ex1,ex2,ex3 => checks that the collection works and that the correct converter is called (java collections)
      def basicJTest(nom:String,ordered:Boolean=true):Unit = {
        val x = get(nom,w)
        x.u(o("ex1","ex2","ex3"))
        val fld = x.read().asInstanceOf[java.util.Collection[Ex.Val]]
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
      //runs basicSTest for fields aNN where NN loops in the list (scala)
      def suiteS(x:Int*):Unit = for (i<-x) basicSTest(s"a$i")
      //runs basicJTest for fields aNN where NN loops in the list (java)
      def suiteJ(x:Int*):Unit = for (i<-x) basicJTest(s"a$i")
      
      get("a1",w).u(o("1","2","3"))
      assert(w.a1.contains(1))
      assert(w.a1.contains(2))
      assert(w.a1.contains(3))
      assert(!w.a1.contains(4))
      get("a3",w).u(o("ex1","ex2"))
      assert(w.a3.contains(Ex.ex1))
      assert(w.a3.contains(Ex.ex2))
      assert(!w.a3.contains(Ex.ex3))
      get("a4",w).u(o("01/01/2010 00:00","01/01/2011 00:00","01/01/2012 00:00","01/01/2012 00:00"))
      assert(w.a4.contains(new java.util.Date("01/01/2010 00:00")))
      assert(w.a4.contains(new java.util.Date("01/01/2011 00:00")))
      assert(w.a4.contains(new java.util.Date("01/01/2012 00:00")))
      assert(w.a4.size==3)
      get("a5",w).u(o("ex1","ex2"))
      assert(w.a5.front==Ex.ex1)
      assert(w.a5.length==2)
      get("a7",w).u(o("ex2","ex2","ex1"))
      assert(w.a7.exists(_==Ex.ex1))
      assert(!w.a7.exists(_==Ex.ex3))
      assert(w.a7.last==Ex.ex1)
      assert(w.a7.size==3)
      get("a9",w).u(o("ex1","ex2","ex3","ex2"))
      assert(w.a9.top==Ex.ex1)
      assert(w.a9.contains(Ex.ex3))
      assert(w.a9.size==4)
      suiteS(2,6,8,10,11,12,13,14,28,30,31,33,34,35,36,37,38,39,40,41,42,44,45,46,47,49,50,52)
      get("a17",w).u(o("1","3","5"))
      assert(w.a17.get(1))
      assert(w.a17.get(3))
      assert(w.a17.get(5))
      assert(!w.a17.get(0))
      assert(!w.a17.get(6))
      get("a19",w).u(o("a1","b1","c1"))
      get("a32",w).u(o("3","7","11"))
      assert(w.a32.contains(11))
      assert(w.a32.contains(7))
      assert(w.a32.contains(3))
      assert(!w.a32.contains(4))
      get("a51",w).u(o((Ex.ex1,null),(Ex.ex3,Ex.ex1),(Ex.ex2,Ex.ex1)))
      assert(w.a51.iterator.corresponds(Array(Ex.ex1,Ex.ex3,Ex.ex2))(_._2==_))  //for some reason, history inverts the parameters
      val l=new java.util.LinkedList[MyEnum](); l.add(MyEnum.a1); l.add(MyEnum.b1); l.add(MyEnum.c1)
      assert(w.a19.containsAll(l))
      suiteJ(15,16,21,22,23,24,27)
      basicJTest("a20",false)
      basicJTest("a26",false)
      val h1 = new Histo("h1")
      val h2 = new Histo("h2")
      val h3 = new Histo("h2")
      get("a48",w).u(o((h1,Ex.ex1),(h2,Ex.ex2),(h3,Ex.ex3)))
      assert(w.a48.iterator.corresponds(Array(Ex.ex1,Ex.ex2,Ex.ex3))(_._1==_))  //for some reason, history inverts the parameters
      out0.println(w)
    }
  }
  /** Test (elementary) for binders using Seq.
   */
  @Test class BinderMixedEncapTest extends StandardTester {
    //nasty multi-encapsulation of several layers of maps/seqs
    class W(val a1:Map[Integer,List[String]], val a2:Array[Map[Integer,java.util.EnumMap[MyEnum,List[java.util.Properties]]]])
    import loader.commons._  //make --> visible
    def apply(file:Solver,out0:PrintWriter) = {
      import Helper._
      val w = new W(null,null)
      
      get("a1",w).u (
        u("1")(o("x","y","z")),
        u("2")(o("u","v","w"))
      )
      get("a2",w).u (u( //Array
          u("1") (      //Map
            u("a1")     //Map
              ( u(o("v1" --> "p1", "v2" --> "p2")), u(o("v3" --> "p3", "v4" --> "p4") )),
            u("b1")
              ( u(o("v5" --> "p5", "v6" --> "p6")), u(o("v7" --> "p7", "v8" --> "p8") ))
          ),
          u("2") (
            u("c1") (u(o("v11" --> "p11", "v12" --> "p12")),u(o("v13" --> "p13", "v14" --> "p14")) ),
            u("a1") (u(o("v15" --> "p15", "v16" --> "p16")),u(o("v17" --> "p17", "v18" --> "p18")) )
          )
      ))
      
      out0.println(w.a1)
      out0.println(write(w.a2))
    }
  }  
  
  //XXX next task: add converters for java.util classes
  //XXX next task: include TreeXXX using inherited order
}

