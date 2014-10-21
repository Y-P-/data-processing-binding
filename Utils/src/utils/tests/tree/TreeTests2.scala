package utils.tests.tree

import scala.collection.GenTraversable
import utils.tree2._
import java.io.PrintWriter
import utils.LogTester._
import org.junit.Test


object TreeTests2 {
  /*
   * Testing with tree:
   * /(7) => { d/4 => { a/1, b/2 }, e/5 => { c/3 }, f/6 => { d/4 => { a/1, b/2 }, x/5 => { c/3 } } }
   */
  val c1  = StringTree(1)
  val c2  = StringTree(2)
  val c3  = StringTree(3)
  val c11 = StringTree(4,Seq("a"->c1,"b"->c2))
  val c12 = StringTree(5,Seq("c"->c3))
  val c13 = StringTree(6,"d"->c11,"x"->c12)
  val m   = StringTree(7,Seq("d"->c11,"e"->c12,"f"->c13))

  /*
   * The same basic tree with complex default everywhere.
   * This is an extremely convoulted tree which loops on itself in several ways.
   */
  val (m1,amap1) = {
    var c1:StringTree[Int]  = null
    var c2:StringTree[Int]  = null
    var c3:StringTree[Int]  = null
    var c11:StringTree[Int] = null
    var c12:StringTree[Int] = null
    var c13:StringTree[Int] = null
    var m:StringTree[Int]   = null
    val f1: String=>StringTree[Int] = {
      case "a" => c1
      case "x" => c2
      case _   => m
    }
    val f2: String=>StringTree[Int] = {
      case "v" => c3
      case "w" => c11
      case _   => m
    }
    val f3: String=>StringTree[Int] = {
      case "u" => c13
      case "x" => c12
      case _   => m
    }
    val f4: String=>StringTree[Int] = {
      case "o" => m
      case "r" => c12
      case _   => c1
    }
    c1  = StringTree(1,f1)
    c2  = StringTree(2,f2)
    c3  = StringTree(3,f3)
    c11 = StringTree(4,Seq("a"->c1,"b"->c2),f4)
    c12 = StringTree(5,Seq("c"->c3),f1)
    c13 = StringTree(6,Seq("d"->c11,"x"->c12),f2)
    m   = StringTree(7,Seq("d"->c11,"e"->c12,"f"->c13),f3)
    (m,Array(m,c1,c2,c3,c11,c12,c13)) //an array for mapping operation from Int => StringTree[Int]
  }  
  
  val amap = Array(m,c1,c2,c3,c11,c12,c13) //an array for mapping operation from Int => StringTree[Int]
  
  def mapper(i:Int) = amap(i-1).seqView()
  def mapper1(i:Int) = amap(i-1)
  
  //tests the toString operation
  def testPrint(implicit out:PrintWriter) = out.println(m)
  
  //tests the map operation to PrefixTree[String,String]
  def testMap1(implicit out:PrintWriter)  = out.println(m.map[String,PrefixTree[String,String]]((_:Int).toString+"y"))
  
  //tests the map operation to StringTree[String]
  def testMap2(implicit out:PrintWriter)  = out.println(m.map[String,StringTree[String]]((_:Int).toString+"y"))
  
  //tests the map operation to PrefixTree[Int,String] (full mapping of key and value)
  def testMap3(implicit out:PrintWriter)  = out.println(m.mapFull[Int,String,PrefixTree[Int,String]](_(0)-'a',null,_.toString+"y"))
  
  //tests filterAll
  def testFilter1(implicit out:PrintWriter) = out.println(m.filterAll(_._1!="b"))
  
  //tests get/apply
  def testGet(implicit out:PrintWriter) = {
    out.println(m.get("d"))
    out.println(m("d"))
  }
  
  //tests get/apply on SeqView
  def testGetSeq(implicit out:PrintWriter) = {
    import out._
    val v = m.seqView()
    println(v.get("d","a"))
    println(v("d","a"))
    println(v(Seq("d","a")))
    println(v.get("d","a","c"))
    try { println(v("d","a","c")) } catch { case e:java.util.NoSuchElementException => println("d.a.c not found")}
  }
  
  //tests seqView
  def testSeqView(implicit out:PrintWriter) = m.seqView().foreach(out.println)
  
  //tests FlatMap for SeqView
  def testSeqFlatMap(implicit out:PrintWriter) = StringTree.builder[Int](m.seqView().flatMap(mapper _)).seqView().foreach(out.println)
  
  //tests FlatMap for SeqView
  def testFlatMap(implicit out:PrintWriter) = m.flatMap[Int,StringTree[Int]](mapper1 _).seqView().foreach(out.println)

  //tests FlatMap for map with default
  def testBasicDefault(implicit out:PrintWriter) = {
    out.println(m1("x").value)            //5
    out.println(m1("x")("a").value)       //1
    out.println(m1("x")("a")("z").value)  //7
  }

  //tests FlatMap for map with default
  def testDefFlatMap(implicit out:PrintWriter) = {
    //we have to take a looks at the flat development of the tree with no default first
    //we remember that flatMap enriches the existing tree, and replaces the current value with the value of the mapped tree
    val m2 = m1.flatMap[Int,StringTree[Int]]((i:Int)=>amap1(i-1))
    //the value of the map for 7 will replace m1 value => 6
    out.println(m2.value)
    //"x" is not a key for m1, but it is for c13 which was expended straight into m1 : m2("x") = c13("x") = c12
    out.println(m2("x").value)            //value for c12 (5)
    out.println(m2("x")("a").value)       //'a' is a default for c12 => f1(a) = c1 (1)
    out.println(m2("x")("a")("z").value)  //'z' is no value for c1 => f1(z) = old top (7)  --remember: this subtree is expanded straight ; no subexpension within
    out.println(m2("x")("a")("x").value)  //'x' is no value => f1(x) = c2 (2)
    //"o" is not a key in m1, nor in map(7) ; its value is m1.default("o") = f3("o") = m1, expanded with flatmap => value of 6
    //now, this default behaves as above : the transformation happens once only
    //the itch of course is that each call to m2("o") rebuilds the image
    out.println(m2("o").value)
    out.println(m2("o")("x").value)            //value for c12 (5)
    out.println(m2("o")("x")("a").value)       //'a' is a default for c12 => f1(a) = c1 (1)
    out.println(m2("o")("x")("a")("z").value)  //'z' is no value for c1 => f1(z) = old top (7)  --remember: this subtree is expanded straight ; no subexpension within
    out.println(m2("o")("x")("a")("x").value)  //'x' is no value => f1(x) = c2 (2)
    //check that the flapMap propagates down
    out.println(m2("d").value)                 //was 4, replaced by 3
    out.println(m2("d")("a").value)            //was 1, replaced by 7
    out.println(m2("d")("a")("x").value)       //apply flatMap on f1("x")=c2 => c1 (1)
    out.println(m2("d")("a")("a").value)       //apply flatMap on f1("a")=c1 => old top (7)
    out.println(m2("d")("a")("o").value)       //apply flatMap on f1("o")=old top => c13 (6)
  }
  
  def testBuildFromCanonical(implicit out:PrintWriter) = out.println(StringTree.builder[Int](Seq(
      (Seq("x","y"),1),
      (Seq("x","y","z"),2),
      (Seq("x"),3),
      (Seq("x","v"),4),
      (Seq("z"),5),         //Note: squashed by the 7 below
      (Seq("z","a","b"),6),
      (Seq("z"),7)
    )))
    
  def testConstant(implicit out:PrintWriter) = {
    val c = StringTree.builder[Int].constant(3)
    //test that constant works
    out.println(c)
    out.println(c("a"))
    out.println(c("a")("b")("c"))
    //tests that constant works on flatmap as expected
    val c2 = StringTree.builder[Double].constant(5.01)
    val c1 = c.flatMap[Double,StringTree[Double]]((i:Int)=> if (i==3) c2 else null)
    out.println(c1)
    out.println(c1("a"))
    out.println(c1("a")("b")("c"))
  }
  
  def testConstantMap(implicit out:PrintWriter) = {
    val c = StringTree.builder[Int].constant(3).map[String,StringTree[String]]((_:Int).toString)
    out.println(c)
    out.println(c("a"))
    out.println(c("a")("b")("c"))
  }
  
  def testBasicZip(implicit out:PrintWriter) = {
    //running zip against itself shows that zip basically works
    val r = m1.zip[String,StringTree[Int],StringTree[String]](m, (t1,t2) =>
      for (v1<-t1.value; v2<-t2.value) yield s"$v1-$v2"
    )
    out.println(r)
    out.println(r("e"))
  }
  
  @Test class TreeTest2 extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      implicit val o = out
      import out._
      println("----01--------")
      testPrint
      println("----02--------")
      testMap1
      println("----03--------")
      testMap2
      println("----04--------")
      testMap3
      println("----05--------")
      testFilter1
      println("----06--------")
      testSeqView
      println("----07--------")
      testGet
      println("----08--------")
      testGetSeq
      println("----09--------")
      testBuildFromCanonical
      println("----10--------")
      testSeqFlatMap
      println("----11--------")
      testFlatMap
      println("----12--------")
      testConstant
      println("----13--------")
      testConstantMap
      println("----14--------")
      testBasicDefault
      println("----15--------")
      testDefFlatMap
      println("----16--------")
      testBasicZip
      //val r1 = StringTree.builder[Int](t1.seqView().flatMap(mapper _).toBuffer)
      //println(r1.seqView().mkString("\n"))
    }
  }
}