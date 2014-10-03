package utils.tree2

import scala.collection.GenTraversable


object X {
  /*
   * Testing with tree:
   * /(7) => { d/4 => { a/1, b/2 }, e/5 => { c/3 }, f/6 => { d/4 => { a/1, b/2 }, x/5 => { c/3 } } }
   */
  val c1 = StringTree(1)
  val c2 = StringTree(2)
  val c3 = StringTree(3)
  val c11 = StringTree(4,Map("a"->c1,"b"->c2))
  val c12 = StringTree(5,Map("c"->c3))
  val c13 = StringTree(6,"d"->c11,"x"->c12)
  val m = StringTree(7,Map("d"->c11,"e"->c12,"f"->c13))

  val amap = Array(m,c1,c2,c3,c11,c12,c13) //an array for mapping operation from Int => StringTree[Int]
  
  def mapper(i:Int) = amap(i-1).seqView()
  def mapper1(i:Int) = amap(i-1)
  
  implicit val replace = true
  
  //tests the toString operation
  def testPrint() = println(m)
  
  //tests the map operation to PrefixTree[String,String]
  def testMap1()  = println(m.map[String,PrefixTree[String,String]]((_:Int).toString+"y"))
  
  //tests the map operation to StringTree[String]
  def testMap2()  = println(m.map[String,StringTree[String]]((_:Int).toString+"y"))
  
  //tests the map operation to PrefixTree[Int,String] (full mapping of key and value)
  def testMap3()  = println(m.mapFull[Int,String,PrefixTree[Int,String]](_(0)-'a',_.toString+"y"))
  
  //tests filterAll
  def testFilter1() = println(m.filterAll(_._1!="b"))
  
  //tests get/apply
  def testGet() = {
    println(m.get("d"))
    println(m("d"))
  }
  
  //tests get/apply on SeqView
  def testGetSeq() = {
    val v = m.seqView()
    println(v.get("d","a"))
    println(v("d","a"))
    println(v(Seq("d","a")))
    println(v.get("d","a","c"))
    try { println(v("d","a","c")) } catch { case e:java.util.NoSuchElementException => println("d.a.c not found")}
  }
  
  //tests seqView
  def testSeqView() = m.seqView().foreach(println)
  
  //tests FlatMap for SeqView
  def testSeqFlatMap() = StringTree.builder[Int](m.seqView().flatMap(mapper _)).seqView().foreach(println)
  
  //tests FlatMap for SeqView
  def testFlatMap() = m.flatMap[Int,StringTree[Int]](mapper1 _).seqView().foreach(println)
  
  def testBuildFromCanonical() = println(StringTree.builder[Int](Seq(
      (Seq("x","y"),1),
      (Seq("x","y","z"),2),
      (Seq("x"),3),
      (Seq("x","v"),4),
      (Seq("z"),5),         //Note: squashed by the 7 below
      (Seq("z","a","b"),6),
      (Seq("z"),7)
    )))
  
  def main(a:Array[String]):Unit = {
    testPrint()
    testMap1()
    testMap2()
    testMap3()
    testFilter1()
    testSeqView()
    testGet()
    testGetSeq()
    testBuildFromCanonical()
    testSeqFlatMap()
    testFlatMap()
    //val r1 = StringTree.builder[Int](t1.seqView().flatMap(mapper _).toBuffer)
    //println(r1.seqView().mkString("\n"))
  }
}

/*
(List(),7)
  (List(),6)
  (List(d),4)
  (List(d, a),1)
  (List(d, b),2)
  (List(x),5)
  (List(x, c),3)
(List(d),4)
(List(d, a),1)
  (List(),7)
  (List(d),4)
  (List(d, a),1)
  (List(d, b),2)
  (List(e),5)
  (List(e, c),3)
  (List(f),6)
  (List(f, d),4)
  (List(f, d, a),1)
  (List(f, d, b),2)
  (List(f, x),5)
  (List(f, x, c),3)
(List(d, b),2)
(List(e),5)
  (List(d),4)
  (List(d, a),1)
  (List(d, b),2)
(List(e, c),3)
(List(f),6)
  (List(e),5)
  (List(e, c),3)
(List(f, d),4)
(List(f, d, a),1)
  (List(),7)
  (List(d),4)
  (List(d, a),1)
  (List(d, b),2)
  (List(e),5)
  (List(e, c),3)
  (List(f),6)
  (List(f, d),4)
  (List(f, d, a),1)
  (List(f, d, b),2)
  (List(f, x),5)
(List(f, d, b),2)
(List(f, x, c),3)
(List(f, x),5)
  (List(d),4) 
  (List(d, a),1)
  (List(d, b),2)
(List(f, x, c),2)
*/