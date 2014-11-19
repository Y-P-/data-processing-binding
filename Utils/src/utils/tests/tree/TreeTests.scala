package utils.tests.tree

import scala.collection.GenTraversable
import utils.tree._
import java.io.PrintWriter
import utils.LogTester._
import org.junit.Test
import scala.collection.GenTraversableOnce
import utils.tree.PrefixTraversableOnce.PullAdapter
import scala.concurrent.Await

/** Tests on trees.
 *  Most tests are done on the 'local' StringTree subclass of PrefixTree.
 *  It shows that trees work even on sub-classes. As StringTree is an almost exact
 *  copy of PrefixTree, tests on StringTree also validate PrefixTree.
 */
object TreeTests {
  // test stripEmpty
  
  import scala.language.implicitConversions
  /*
   * Testing with tree:
   * /(7) => { d/4 => { a/1, b/2 }, e/5 => { c/3 }, f/6 => { d/4 => { a/1, b/2 }, x/5 => { c/3 } } }
   */
  val c1:StringTree[Int]  = StringTree(Some(1))
  val c2  = StringTree(2)
  val c3  = StringTree(3)
  val c11 = StringTree(4,Seq("a"->c1,"b"->c2))
  val c12 = StringTree(5,"c"->c3)
  val c13 = StringTree(6,"d"->c11,"x"->c12)
  val m   = StringTree(7,Seq("d"->c11,"e"->c12,"f"->c13))
  //an extract: (7) => { d/4 => { a/1 }, f/6 => { d/4 => { b/2 } } }
  val m0  = StringTree(7,Seq("d"->StringTree(4,Seq("a"->c1),(x:String)=>m),"f"->StringTree(6,"d"->StringTree(4,Seq("b"->c2)))))

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
  
  //tests filterView
  def testFilter2(implicit out:PrintWriter) = out.println(m.filterView(_._1!="b"))
  
  //tests get/apply
  def testGet(implicit out:PrintWriter) = {
    out.println(m.get("d"))
    out.println(m("d"))
  }
  
  //tests get/apply on SeqView
  def testGetSeq(implicit out:PrintWriter) = {
    val v = m.seqView()
    out.println(v.get("d","a"))
    out.println(v("d","a"))
    out.println(v(Seq("d","a")))
    out.println(v.get("d","a","c"))
    out.printExc(v("d","a","c"))
  }
  
  //tests seqView
  def testSeqView(implicit out:PrintWriter) = m.seqView().foreach(out.println)
  
  //tests FlatMap for SeqView
  def testSeqFlatMap(implicit out:PrintWriter) = StringTree.fromFlat(m.seqView().flatMap(mapper _)).seqView().foreach(out.println)
  
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
  
  def testBuildFromCanonical(implicit out:PrintWriter) = out.println(StringTree.fromFlat(Seq(
      (Seq("x","y"),1),
      (Seq("x","y","z"),2),
      (Seq("x"),3),
      (Seq("x","v"),4),
      (Seq("z"),5),         //Note: squashed by the 7 below
      (Seq("z","a","b"),6),
      (Seq("z"),7)
    )))
    
  def testConstant(implicit out:PrintWriter) = {
    val c = StringTree.constant(3)
    //test that constant works
    out.println(c)
    out.println(c("a"))
    out.println(c("a")("b")("c"))
    //tests that constant works on flatmap as expected
    val c2 = StringTree.constant(5.01)
    val c1 = c.flatMap[Double,StringTree[Double]]((i:Int)=> if (i==3) c2 else null)
    out.println(c1)
    out.println(c1("a"))
    out.println(c1("a")("b")("c"))
  }
  
  def testConstantMap(implicit out:PrintWriter) = {
    val c = StringTree.constant(3).map[String,StringTree[String]]((_:Int).toString)
    out.println(c)
    out.println(c("a"))
    out.println(c("a")("b")("c"))
  }
  
  def testBasicZip(implicit out:PrintWriter) = {
    val r = m1.zip[String,StringTree[Int],StringTree[String]](m1, false, (t1,t2) =>
      for (v1<-t1.value; v2<-t2.value) yield s"$v1-$v2"
    )
    out.println(r)
    out.println(r("e"))
    out.println(r("u"))
    out.println(r("u","v"))
    out.println(r("x"))
    out.println(r("x","c"))
    out.println(r("f","d","a"))
    out.println(r("f","d","a","x"))
  }
  
  def testBasicZipStrict(implicit out:PrintWriter) = {
    val r = m1.zip[String,StringTree[Int],StringTree[String]](m1, true, (t1,t2) =>
      for (v1<-t1.value; v2<-t2.value) yield s"$v1-$v2"
    )
    out.println(r)
    out.println(r("e"))
    out.printExc(r("u"))
    out.printExc(r("u","v"))
    out.printExc(r("x"))
    out.printExc(r("x","c"))
    out.println(r("f","d","a"))
    out.printExc(r("f","d","a","x"))
    //to compare with the next test result where some matches will disappear
    out.println(r("e"))
    out.println(r("d"))
    out.println(r("d","a"))
    out.println(r("d","b"))
  }
  
  def testBasicRestrictZipStrict(implicit out:PrintWriter) = {
    //restrict result using m0 instead of full tree
    val r = m1.zip[String,StringTree[Int],StringTree[String]](m0, true, (t1,t2) =>
      for (v1<-t1.value; v2<-t2.value) yield s"$v1-$v2"
    )
    out.println(r)
    out.printExc(r("e"))     // e not in m0
    out.println(r("d"))      // d in m0
    out.println(r("d","a"))  // a in m0(d)
    out.printExc(r("d","b")) // b not in m0(d)
  }
  
  def testBasicRestrictZip(implicit out:PrintWriter) = {
    //restrict result using m0 instead of full tree
    val r = m1.zip[String,StringTree[Int],StringTree[String]](m0, false, (t1,t2) =>
      for (v1<-t1.value; v2<-t2.value) yield s"$v1-$v2"
    )
    // /(7) => { d/4 => { a/1, b/2 }, e/5 => { c/3 }, f/6 => { d/4 => { a/1, b/2 }, x/5 => { c/3 } } }
    //an extract: (7) => { d/4 => { a/1 }, f/6 => { d/4 => { b/2 } } }
    out.println(r)
    out.printExc(r("e"))         // e not in m0
    out.println(r("d"))          // d in m0
    out.println(r("d","a"))      // a in m0(d)
    out.println(r("f"))          // f in m0
    out.println(r("f","d"))      // d in m0(f)
    out.println(r("f","d","b"))  // b in m0(f,d) 
    out.println(r("d","d"))      // d->d not in m0(d) but defaults to m, d not in m1(d) but defaults to c1 through f4
    out.println(r("d","b","d"))  // that's hotter: in m1, d->b->b=m (following defaults), in m0 this gives c11 (again following defaults)
  }
  
  def testZip2(implicit out:PrintWriter) = {
    //in zip2, we concentrate on checking that the right method is called
    type O = (StringTree[Int],StringTree[Int])=>Option[String]
    val op1:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1+$v2"
    val op2:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1*$v2"
    val op3:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"${(v1+v2)}"
    val opX = PrefixTree.fromFlat(Seq(
          (Seq("d"),op1),
          (Seq("d","a"),op2),
          (Seq("f"),op3),
          (Seq("f","d"),op1),
          (Seq("f","d","b"),op3)
      ))
    //and we check that absent ops yield no result
    val opY = PrefixTree.fromFlat(Seq(
          (Seq("d"),op1),
          (Seq("d","a"),op2),
          (Seq("f","d"),op1),
          (Seq("f","d","b"),op3)
      ))
    val rx = m1.zip2[String,StringTree[Int],PrefixTree[String,O],StringTree[String]](m0,true,opX)
    out.println(rx)
    val ry = m1.zip2[String,StringTree[Int],PrefixTree[String,O],StringTree[String]](m0,true,opY)
    out.println(ry)
  }
  
  def testZip2NonStrictAndFromFlatWithDefault(implicit out:PrintWriter) = {
    //in zip2, we concentrate on checking that the right method is called
    type O = (StringTree[Int],StringTree[Int])=>Option[String]
    val op1:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1+$v2"
    val op2:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1*$v2"
    val op3:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"${(v1+v2)}"
    val opc:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1%$v2"
    
    val opx = PrefixTree.constant[String,O](opc)
    
    val opX = PrefixTree.fromFlat2(Seq(
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(null,null)),
          (Seq("f"),(op3,opx)),
          (Seq("f","d","b"),(op2,opx))
      ))
    //note here that f->d exists but has no op: default is not used, and being not strict, this will
    //not stop the tree exploration. But f->d won't have any value.
    val opY = PrefixTree.fromFlat2(Seq(
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(op2,opx)),
          (Seq("f","d"),(op1,opx))
      ))
    //note here that f exists but has no op: default is not used, and being not strict, this will
    //not stop the tree exploration. But f won't have any value.
    val rx = m1.zip2[String,StringTree[Int],PrefixTree[String,O],StringTree[String]](m0,false,opX)
    out.println(rx)
    val ry = m1.zip2[String,StringTree[Int],PrefixTree[String,O],StringTree[String]](m0,false,opY)
    out.println(ry)
  }

  def testZipFull(implicit out:PrintWriter) = {
    //in zipFull, we do a similar transformation as above, but replace the key with the Int value and Strings with Symbols.
    //the result should be close to the previous one, thus easily comparable.
    //this is far from testing all possibilities, but its a good start.
    type O = (String,StringTree[Int],StringTree[Int])=>(Option[Int],Option[Symbol],Int=>PrefixTree[Int,Symbol])
    val op1:O = (s,t1,t2)=>(t2.value,for (v1<-t1.value; v2<-t2.value) yield Symbol(s"$s$v1+$v2"),null)
    val op2:O = (s,t1,t2)=>(t2.value,for (v1<-t1.value; v2<-t2.value) yield Symbol(s"$s$v1*$v2"),null)
    val op3:O = (s,t1,t2)=>(t2.value,for (v1<-t1.value; v2<-t2.value) yield Symbol(s"$s${(v1+v2)}"),null)
    val opc:O = (s,t1,t2)=>(t2.value,for (v1<-t1.value; v2<-t2.value) yield Symbol(s"$s$v1%$v2"),null)
    
    val opx = PrefixTree.constant[String,O](opc)
    
    val opX = PrefixTree.fromFlat2(Seq(
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(null,null)),
          (Seq("f"),(op3,opx)),
          (Seq("f","d","b"),(op2,opx))
      ))
    val opY = PrefixTree.fromFlat2(Seq(
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(op2,opx)),
          (Seq("f","d"),(op1,opx))
      ))
    //note that f->d has no associated value: the corresponding subtree is excluded (the value becomes the key => no key, no tree...)
    val rx = m1.zipFull[Int,Symbol,StringTree[Int],PrefixTree[String,O],PrefixTree[Int,Symbol]]("",null,m0,PrefixTraversableOnce.NOT_STRICT,opX)
    out.println(rx)
    //note that f has no associated value: itself and its full subtree is excluded
    val ry = m1.zipFull[Int,Symbol,StringTree[Int],PrefixTree[String,O],PrefixTree[Int,Symbol]]("",null,m0,PrefixTraversableOnce.NOT_STRICT,opY)
    out.println(ry)
  }
    
  def testForeach2(implicit out:PrintWriter) = {
    import out.print
    m.deepForeach2[Int](""){(p,t,c)=>
      print(s"${t._1}(${if(t._2.value!=None) t._2.value.get else ""})={")
      var i=0
      while (c.hasNext) { i+=1; c.next }
      print(s"}[$i in ${if (p!=null) p.value.get else "top"}]")
      i
    }
  }
  
  def testPushPull(implicit out:PrintWriter) = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext
    implicit val executionContext = ExecutionContext.global
    val (s,r) = PrefixTraversableOnce[Unit,String,String] { (t,r)=>
      out.print(s"${t._1}={")
      val i=r.foldLeft(0)((i,u)=>i+1)
      out.print(s"}(${if(t._2.value!=None) t._2.value.get else ""})[$i]")
    }
    s.push("a")
    s.pull
    s.push("aa")
    s.pull("1")
    s.pull
    s.push("ab")
    s.pull("2")
    s.push("aab")
    s.pull("3")
    s.push("d")
    s.pull("5")
    s.pull
    s.pull
    s.push("aac")
    s.pull("4")
    s.pull
    s.pull
    s.pull
    Await.result(r, 10 millis)
  }
    
  @Test class TreeTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      implicit val o = out
      import out._
      var i=0
      def t(f: =>Unit) = { println(s"----$i--------"); i+=1; f }
      t(testPrint)
      t(testMap1)
      t(testMap2)
      t(testMap3)
      t(testFilter1)
      t(testFilter2)
      t(testSeqView)
      t(testGet)
      t(testGetSeq)
      t(testBuildFromCanonical)
      t(testSeqFlatMap)
      t(testFlatMap)
      t(testConstant)
      t(testConstantMap)
      t(testBasicDefault)
      t(testDefFlatMap)
      t(testBasicZip)
      t(testBasicZipStrict)
      t(testBasicRestrictZipStrict)
      t(testBasicRestrictZip)
      t(testZip2)
      t(testZip2NonStrictAndFromFlatWithDefault)
      t(testZipFull)
      t(testForeach2)
      t(testPushPull)
      //navigable
      //navigables in zip/other operations
      //foreach
      //references
      //references in zip/other operations
      //val r1 = StringTree.builder[Int](t1.seqView().flatMap(mapper _).toBuffer)
      //println(r1.seqView().mkString("\n"))
    }
  }
}
