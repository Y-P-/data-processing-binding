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
  val c11 = StringTree(4,Map("a"->c1,"b"->c2))
  val c12 = StringTree(5,Map("c"->c3))
  val c13 = StringTree(6,"d"->c11,"x"->c12)
  val m   = StringTree(7,Map("d"->c11,"e"->c12,"f"->c13))

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
    out.println(c(""))
    out.println(c("a"))
    out.println(c("a")("b")("c"))
  }
  
  @Test class TreeTest2 extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      implicit val o = out
      testPrint
      testMap1
      testMap2
      testMap3
      testFilter1
      testSeqView
      testGet
      testGetSeq
      testBuildFromCanonical
      testSeqFlatMap
      testFlatMap
      testConstant
      //val r1 = StringTree.builder[Int](t1.seqView().flatMap(mapper _).toBuffer)
      //println(r1.seqView().mkString("\n"))
    }
  }
}
