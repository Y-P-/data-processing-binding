package utils.tests.tree

import scala.collection.GenTraversable
import utils.tree._
import java.io.PrintWriter
import utils.LogTester._
import org.junit.Test
import scala.collection.GenTraversableOnce
import utils.tree.PushPull.PullAdapter
import scala.concurrent.Await

/** Tests on trees.
 *  Most tests are done on the 'local' StringTree subclass of PrefixTree.
 *  It shows that trees work even on sub-classes. As StringTree is an almost exact
 *  copy of PrefixTree, tests on StringTree also validate PrefixTree.
 */
class TreeTests extends StandardTester {
	import TreeTests._
  @Test override def test():Unit = super.test()
	def apply(file:Solver,out:PrintWriter) = {
		implicit val o = out
		import out._
		def t(i:Int,f: =>Unit) = { println(s"--------$i--------"); f }
		t(0,testPrint)
		t(1,testMap1)
		t(2,testMap2)
		t(3,testMap3)
		t(4,testFilter1)
		t(5,testFilter2)
		t(6,testSeqView)
		t(7,testGet)
		t(8,testBuildFromCanonical)
		t(9,testSeqFlatMap)
    t(10,testFlatMap)
    t(11,testFlatMap1)
		t(12,testConstant)
		t(13,testConstantMap)
		t(14,testBasicDefault)
		t(15,testBasicZip)
		t(16,testBasicZipStrict)
		t(17,testBasicRestrictZipStrict)
		t(18,testBasicRestrictZip)
		t(19,testZip2)
		t(20,testZip2NonStrictAndFromFlatWithDefault)
		t(21,testZipFull)
		t(22,testZipFullView)
		t(23,testForeach)
		t(24,testFold)
		t(25,testPushPull)
    t(26,testPartition)
    t(27,testMutable)
    t(28,testInfinite)
    t(29,testDOM1)
    t(30,testDOM2)
    t(31,testDOM3)
    t(32,testDOM4)
	}
}

object TreeTests {
  // test stripEmpty

  import scala.language.implicitConversions
  /*
   * Testing with tree:
   * /(7) => { d/4 => { a/1, b/2 }, e/5 => { c/3 }, f/6 => { d/4 => { a/1, b/2 }, x/5 => { c/3 } } }
   */
  val x = StringTree(1)  //note that with the implicit toBuilder in Factory2, we can use this; using a builder as below is preferred

  val sbd = StringTree.builder[Int]
  val c1:StringTree[Int] = sbd(Some(1))
  val c2  = sbd(2)
  val c3  = sbd(3)
  val c11 = sbd(4,Seq("a"->c1,"b"->c2))
  val c12 = sbd(5,"c"->c3)
  val c13 = sbd(6,"d"->c11,"x"->c12)
  val m   = sbd(7,Seq("d"->c11,"e"->c12,"f"->c13))
  //an extract: (7) => { d/4 => { a/1 }, f/6 => { d/4 => { b/2 } } }
  val m0  = sbd(7,Seq("d"->sbd(4,Seq("a"->c1),(x:String)=>m),"f"->sbd(6,"d"->sbd(4,Seq("b"->c2)))))

  /*
   * almost same basic tree with complex default everywhere ; c13 differs (subtrees d and x reversed).
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
    c1  = sbd(1,f1)
    c2  = sbd(2,f2)
    c3  = sbd(3,f3)
    c11 = sbd(4,Seq("a"->c1,"b"->c2),f4)
    c12 = sbd(5,Seq("c"->c3),f1)
    c13 = sbd(6,Seq("d"->c12,"x"->c11),f2)
    m   = sbd(7,Seq("d"->c11,"e"->c12,"f"->c13),f3)
    (m,Array(m,c1,c2,c3,c11,c12,c13)) //an array for mapping operation from Int => StringTree[Int]
  }

  val amap = Array(m,c1,c2,c3,c11,c12,c13) //an array for mapping operation from Int => StringTree[Int]

  def mapper(i:Int) = amap(i-1).seqView()

  //tests the toString operation
  def testPrint(implicit out:PrintWriter) = out.println(m)

  //tests the map operation to PrefixTree[String,String]
  def testMap1(implicit out:PrintWriter)  = {
    import PrefixTree._  //we have PrefixTree and StringTree builders in view:choose
    out.println(m.map((_:Int).toString+"y"))
  }
  //tests the map operation to StringTree[String]
  def testMap2(implicit out:PrintWriter)  = {
    import StringTree._  //we have PrefixTree and StringTree builders in view:choose
    out.println(m.map((_:Int).toString+"y"))
  }

  //tests the map operation to PrefixTree[Int,String] (full mapping of key and value)
  def testMap3(implicit out:PrintWriter)  = {
    import PrefixTree._  //we have PrefixTree and StringTree builders in view:choose
    out.println(m.mapFull("x",_(0)-'a',_.toString+"y",null))
  }
  //tests filterAll
  def testFilter1(implicit out:PrintWriter) = out.println(m.filterAll(_._1!="b"))

  //tests filterView
  def testFilter2(implicit out:PrintWriter) = out.println(m.filterView(_._1!="b"))

  //tests get/apply
  def testGet(implicit out:PrintWriter) = {
    out.println(m.get("d"))
    out.println(m("d"))
  }

  //tests seqView
  def testSeqView(implicit out:PrintWriter) = m.seqView().foreach(out.println)

  //tests FlatMap for SeqView
  //note that while the next test looks like it should yield the same results, it does not.
  //this comes from the fact that expansion in sequence view is 'immediate', whereas it is hierarchical
  //in the normal flatMap: hence while we have the same tree structure, some values do differ.
  def testSeqFlatMap(implicit out:PrintWriter) = sbd.fromFlat(m.seqView().flatMap(mapper _)).seqView().foreach(out.println)

  //tests FlatMap
  //For any level, we have:
  // - old keys stand, and their value is now F(old value)
  // - new keys developed as from the transformation for their value (Fv).
  // - overlapping keys merge
  //  F(m)   : d=F(c11), e=F(c12), f=F(c13) // Fv(7)=c13 => d=c11, x=c11 // d=merge(F(c11),c11)
  //  F(c13) : d=F(c11), e=F(c12) // Fv(6)=c12 => c=c3 // no merge
  //  F(c12) : c=F(c3) // Fv(5)=c11 => a=c1, b=c2 // no merge
  //  F(c11) : a=F(c1), b=F(c2) // Fv(4)=c3 // no merge
  //  F(c3)  : no key // Fv(3)=c2 // no merge
  //  F(c2)  : no key // Fv(2)=c1 // no merge
  //  F(13)  : no key // Fv(1)=m1 => d=c11, e=c12, f=c13 // no merge
  // in particular note that key d ends up with value 4 (from the merge with Fv(7)(d)=c11)) ; this applies to d.a (1) and d.b (2) too.
  def testFlatMap(implicit out:PrintWriter) = m.flatMap[Int,StringTree[Int]](MERGE)(i=>amap(i-1)).seqView().foreach(out.println)

  //almost same, but check key 'd' ; in m1, c13 is slightly different. This results for key 'd' in a
  //merge between F(c11) [3,a,b] and c12 [5,c] hence [5,a,b,c]
  //sub key c comes out of c12 and its value is c3 (value 3)
  //sub key b comes out of F(c11) and is F(c2) (no merge) = c1 (no merge) (value 1)
  //sub key a comes out of F(c11) and is F(c1) (no merge) = m1 (no merge) (value 7) : a contains m1 as a proper sub tree
  //for fun, the resulting class is also different
  def testFlatMap1(implicit out:PrintWriter) = out.println(m1.flatMap[Int,PrefixTree[String,Int]](MERGE)(i=>amap1(i-1)).apply("d"))

  //tests FlatMap for map with default
  def testBasicDefault(implicit out:PrintWriter) = {
    out.println(m1("x").value)            //5
    out.println(m1("x")("a").value)       //1
    out.println(m1("x")("a")("z").value)  //7
  }

  def testBuildFromCanonical(implicit out:PrintWriter) = out.println(sbd.fromFlat(Seq(
      (Seq("x","y"),1),
      (Seq("x","y","z"),2),
      (Seq("x"),3),
      (Seq("x","v"),4),
      (Seq("z"),5),         //Note: squashed by the 7 below
      (Seq("z","a","b"),6),
      (Seq("z"),7)
    )))

  def testConstant(implicit out:PrintWriter) = {
    val sbd = StringTree.builder[Int]
    val c = sbd.constant(3)
    //test that constant works
    out.println(c)
    out.println(c("a"))
    out.println(c("a")("b")("c"))
    //tests that constant works on flatmap as expected
    val sbd1 = StringTree.builder[Double]
    val c2 = sbd1.constant(5.01)
    val c1 = c.flatMap[Double,StringTree[Double]](MERGE)((i:Int)=> if (i==3) c2 else null)
    out.println(c1)
    out.println(c1("a"))
    out.println(c1("a")("b")("c"))
  }

  def testConstantMap(implicit out:PrintWriter) = {
    implicit val b0 = StringTree.builder[String]  //we have PrefixTree and StringTree builders in view
    val c = StringTree.constant(3).map((_:Int).toString)
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
    implicit val b0 = StringTree.builder[String]  //we have PrefixTree et StringTree builders in view: let us choose StringTree
    val r = m1.zip(m1, true, (t1:StringTree[Int],t2) =>
      for (v1<-t1.value; v2<-t2.value) yield s"$v1-$v2"
    )
    out.println(r)
    out.println(r("e"))
    out.printExc(r("u"))
    out.printExc(r("u","v"))
    out.printExc(r("x"))
    out.printExc(r("x","c"))
    out.println(r("f","x","a"))
    out.printExc(r("f","x","a","x"))
    //to compare with the next test result where some matches will disappear
    out.println(r("e"))
    out.println(r("d"))
    out.println(r("d","a"))
    out.println(r("d","b"))
  }

  def testBasicRestrictZipStrict(implicit out:PrintWriter) = {
    //restrict result using m0
    implicit val b0 = StringTree.builder[String]  //we have PrefixTree and StringTree builders in view
    val r = m1.zip(m0, true, (t1:StringTree[Int],t2) =>
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
    implicit val b0 = PrefixTree.builder[String,String]  //we have PrefixTree et StringTree builders in view
    val r = m1.zip(m0, false, (t1:StringTree[Int],t2) =>
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
    out.println(r("f","d","b"))  // b in m0(f,d) - uses default for b in m1(f,d)
    out.println(r("d","d"))      // d->d not in m0(d) but defaults to m, d not in m1(d) but defaults to c1 through f4
    out.println(r("d","b","d"))  // that's hotter: in m1, d->b->b=m (following defaults), in m0 this gives c11 (again following defaults)
  }

  def testZip2(implicit out:PrintWriter) = {
    //in zip2, we concentrate on checking that the right method is called
    type O = (StringTree[Int],StringTree[Int])=>Option[String]
    val op1:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1+$v2"
    val op2:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1*$v2"
    val op3:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"${(v1+v2)}"

    val opX = PrefixTree.fromFlat(Seq(   //Using the implicit just for fun
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
    implicit val b0 = PrefixTree.builder[String,String]  //we have PrefixTree and StringTree builders in view
    val rx = m1.zip2(m0,FULL_STRICT,opX)
    out.println(rx)
    val ry = m1.zip2(m0,FULL_STRICT,opY)
    out.println(ry)
  }

  def testZip2NonStrictAndFromFlatWithDefault(implicit out:PrintWriter) = {
    //in zip2, we concentrate on checking that the right method is called
    type O = (StringTree[Int],StringTree[Int])=>Option[String]
    val op1:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1+$v2"
    val op2:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1*$v2"
    val op3:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"${(v1+v2)}"
    val opc:O = (t1,t2)=>for (v1<-t1.value; v2<-t2.value) yield s"$v1%$v2"

    val tbd = PrefixTree.builder[String,O]
    val opx = tbd.constant(opc)

    val opX = tbd.fromFlat2(Seq(
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(null,null)),
          (Seq("f"),(op3,opx)),
          (Seq("f","d","b"),(op2,opx))
      ))
    //note here that f->d exists but has no op: default is not used, and being not strict, this will
    //not stop the tree exploration. But f->d won't have any value.
    val opY = tbd.fromFlat2(Seq(
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(op2,opx)),
          (Seq("f","d"),(op1,opx))
      ))
    //note here that f exists but has no op: default is not used, and being not strict, this will
    //not stop the tree exploration. But f won't have any value.
    implicit val b0 = StringTree.builder[String]  //we have PrefixTree et StringTree builders in view
    val rx = m1.zip2(m0,NOT_STRICT,opX)           //has default
    out.println(rx)
    val ry = m1.zip2(m0,NOT_STRICT,opY)           //has no default
    out.println(ry)
    //out.println(rx("f")("f"))
  }

  def testZipFull(implicit out:PrintWriter) = {
    //in zipFull, we do a similar transformation as above, but replace the key with the Int value and Strings with Symbols.
    //the result should be close to the previous one, thus easily comparable.
    //this is far from testing all possibilities, but its a good start.
    abstract class F extends (Seq[((String,StringTree[Int]),(StringTree[Int],PrefixTree[String,F]))]=>(Int,Option[Symbol],Int=>PrefixTree[Int,Symbol])) {
      def merge(x:Int, y:Int):String
      def apply(s:Seq[((String,StringTree[Int]),(StringTree[Int],PrefixTree[String,F]))])= {
        val cur=s.head; val t2=cur._2._1; val t1=cur._1._2; val o=cur._2._2
        (t1.value.get,for (v1<-t2.value; v2<-t1.value) yield Symbol(s"${s.head._1._1}${merge(v1,v2)}"),null)
      }
    }
    val op1:F = new F { def merge(x:Int, y:Int) = s"$x+$y" }
    val op2:F = new F { def merge(x:Int, y:Int) = s"$x*$y" }
    val op3:F = new F { def merge(x:Int, y:Int) = s"${x+y}" }
    val opc:F = new F { def merge(x:Int, y:Int) = s"$x%$y" }

    val tbd = PrefixTree.builder[String,F]
    val opx = tbd.constant(opc)

    val opX = tbd.fromFlat2(Seq(
          (Seq(),(op1,opx)),
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(null,null)),
          (Seq("f"),(op3,opx)),
          (Seq("f","d","b"),(op2,opx))
      ))
    val opY = tbd.fromFlat2(Seq(
          (Seq(),(op1,opx)),
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(op2,opx)),
          (Seq("f","d"),(op1,opx))
      ))
    //note that f->d has no associated value: the corresponding subtree is excluded (the value becomes the key => no key, no tree...)
    out.println(m1.zipFullRec("",NOT_STRICT,m0,opX))
    //note that f has no associated value: itself and its full subtree is excluded
    out.println(m1.zipFullRec("",NOT_STRICT,m0,opY))
  }
  def testZipFullView(implicit out:PrintWriter) = {
    //This test must give the same result as testZipFull
    abstract class F extends (Seq[((String,StringTree[Int]),(StringTree[Int],PrefixTree[String,F]))]=>(Int,Option[Symbol])) {
      def merge(x:Int, y:Int):String
      def apply(s:Seq[((String,StringTree[Int]),(StringTree[Int],PrefixTree[String,F]))])= {
        val cur=s.head; val t2=cur._2._1; val t1=cur._1._2; val o=cur._2._2
        (t1.value.get,for (v1<-t2.value; v2<-t1.value) yield Symbol(s"${s.head._1._1}${merge(v1,v2)}"))
      }
    }
    val op1:F = new F { def merge(x:Int, y:Int) = s"$x+$y" }
    val op2:F = new F { def merge(x:Int, y:Int) = s"$x*$y" }
    val op3:F = new F { def merge(x:Int, y:Int) = s"${x+y}" }
    val opc:F = new F { def merge(x:Int, y:Int) = s"$x%$y" }

    val tbd = PrefixTree.builder[String,F]
    val opx = tbd.constant(opc)

    val opX = tbd.fromFlat2(Seq(
          (Seq(),(op1,opx)),
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(null,null)),
          (Seq("f"),(op3,opx)),
          (Seq("f","d","b"),(op2,opx))
      ))
    val opY = tbd.fromFlat2(Seq(
          (Seq(),(op1,opx)),
          (Seq("d"),(op1,opx)),
          (Seq("d","a"),(op2,opx)),
          (Seq("f","d"),(op1,opx))
      ))
    //note that f->d has no associated value: the corresponding subtree is excluded (the value becomes the key => no key, no tree...)
    out.println(m1.zipFullRecView("",NOT_STRICT,m0,opX))
    //note that f has no associated value: itself and its full subtree is excluded
    out.println(m1.zipFullRecView("",NOT_STRICT,m0,opY))
  }

  def testForeach(implicit out:PrintWriter) = {
    import out.print
    type F = (Seq[(String,StringTree[Int])],=>Unit)=>Unit
    val op:String=>F = (info) => (p,recur) => {
      val t = p.head
      print(s"${t._1}(${if(t._2.value!=None) t._2.value.get else ""})={")
      recur
      print(s"}[$info]")
    }
    val op1 = op("X")
    val op2 = op("Y")
    val tbd = PrefixTree.builder[String,F]
    val opX = tbd.fromFlat2(Seq(
        (Seq(),(op1,PrefixTree.constant[String,F](op1))),
        (Seq("f"),(op2,PrefixTree.constant[String,F](op2)))
      ))
    m.deepForeachZipRec("")(opX)
    out.println
    m.deepForeachRec(""){(p,recur)=>
      val t = p.head
      print(s"${t._1}(${if(t._2.value!=None) t._2.value.get else ""})={")
      recur
      print(s"}[in ${if (p.size>1) p(1)._2.value.get else "top"}]")
    }
    out.println
    m.deepForeach(""){(t,recur)=>
      print(s"${t._1}(${if(t._2.value!=None) t._2.value.get else ""})={")
      recur
      print("}")
    }
    out.println
    m.deepForeach(""){(t,recur)=>
      print(s"${t._1}(${if(t._2.value!=None) t._2.value.get else ""})={")
      recur
      print(s"}")
    }
    out.println
  }

  def testFold(implicit out:PrintWriter) = {
    import out.print
    type F = (String,Seq[(String,StringTree[Int])])=>String
    val op:String=>F = (info) => (u,ctx) => {
      val t = ctx.head
      u+t._1+t._2.value+info
    }
    val op1 = op("X")
    val op2 = op("Y")
    val tbd = PrefixTree.builder[String,F]
    val opX:PrefixTree[String,F] = tbd.fromFlat2(Seq(
        (Seq(),(op1,PrefixTree.constant[String,F](op1))),
        (Seq("f"),(op2,PrefixTree.constant[String,F](op2)))
      ))
    out.println(m.deepFoldZipRec("->","",false)(opX))
    out.println(m.deepFoldLeftRec("->","",false)(op("Z")))
    out.println(m.deepFoldLeft("->","",false)((u,ctx)=>op("W")(u,Seq(ctx))))
  }

  def testPushPull(implicit out:PrintWriter) = {
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext
    implicit val executionContext = ExecutionContext.global
    val (s,r) = PushPull[Unit,String,String] { (t,recur)=>
      out.print(s"${t._1}={")
      recur
      out.print(s"}(${if(t._2.value!=None) t._2.value.get else ""})")
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

  def testPartition(implicit out:PrintWriter) = {
    //partition even/odd
    val x = m.partition(null)(x => x._2.value.map(_ % 2 == 0).getOrElse(false))
    out.println(x)
    //rebuild tree (order differs obviously)
    out.println(x._1.merge(x._2,false,(x,y)=>if (x==None) y else x,false))
  }

  def testMutable(implicit out:PrintWriter) = {
    //rebuild tree using mutability
    val bd = MutablePrefixTree.builder[String,Int]
    val xm = bd(7)
    val xc1 = bd(Some(1))
    val xc2 = bd()
    val xc3 = bd(3)
    val xc11 = bd(4)
    val xc12 = bd(5)
    val xc13 = bd(6)
    xc2.value = Some(2)
    xc11.tree += (("a",xc1))
    xc11.tree.put("b",xc2)
    val t = scala.collection.mutable.HashMap[String,MutablePrefixTree[String,Int]]("c"->xc3)
    xc12.tree = t
    xc13.tree ++= Seq("d"->xc11, "x"->xc12)
    xm.tree ++= Seq("d"->xc11,"e"->xc12,"f"->xc13)
    out.println(xm)
    //check copy operation on this basic case
    val xm1 = xm.xcopy[Int,MutablePrefixTree[String,Int]]
    out.println(xm1)
    out.println(xm1("d") eq xm1("f","d"))
    out.println(xm1("e") eq xm1("f","x"))
  }

  def testInfinite(implicit out:PrintWriter) = {
    //rebuild tree using mutability
    val bd = MutablePrefixTree.builder[String,Int]
    val xm1 = bd(1)
    val xm2 = bd(2)
    val xm3 = bd(3)
    xm1("a") = xm2
    xm2("a") = xm3
    xm3("a") = xm1  //rotating dependency
    xm1("b") = xm1  //self dependency
    xm2("b") = xm1  //another loop
    xm3("b") = xm2  //and another
    xm1("c") = bd(4) //new layer
    xm1("c")("a") = xm2 //another cycle at a lower level
    xm1("c")("b") = xm1 //another cycle at a lower level
    xm1("c")("c") = xm3 //another cycle at a lower level
    xm3("c") = xm1("c") //yet another
    //check copy operation on this complex case (no standard print! would loop forever)
    val x1 = xm1.xcopy[Int,MutablePrefixTree[String,Int]]
    val x2 = x1("a")
    val x3 = x2("a")
    out.println(x3("a") eq x1)
    out.println(x1("b") eq x1)
    out.println(x2("b") eq x1)
    out.println(x3("b") eq x2)
    out.println(x1("c","a") eq x2)
    out.println(x1("c","b") eq x1)
    out.println(x1("c","c") eq x3)
    out.println(x1("c") eq x3("c"))
    //check that not everything is equal!!!
    out.println(x1 eq x2)
    out.println(x1 eq x3)
    out.println(x2 eq x3)
    //check foreach: this will write a flat representation of the infinite tree
    xm1.xdeepForeach("z") { (kv,loop)=>
      out.print(s"${kv._1}->(${kv._2.value}){")
      loop
      out.print(s"}")
    }
    out.println
    //check fold: this should write the string of all node values, in the same order as in the previous foreach
    out.println(xm1.xdeepFoldLeft("","z",true) { (u,kv)=> u+kv._2.value.get })
    //idem, but using the down->up order
    out.println(xm1.xdeepFoldLeft("","z",false) { (u,kv)=> u+kv._2.value.get })
  }

  def testDOM(implicit out:PrintWriter, params:DOMPrefixTree.P0[Int], findXpath:String*) = {
    params.doc.createElementNS("urn:my-namespace", "x")
    //copy m to DOMPrefixTree
    val dom = m.copy[Int,DOMPrefixTree[Int]]
    //print as xml with attributes
    dom.asXml(out, false)
    out.println
    //check that actual values are accessible
    out.println(dom("f","d","a").value)
    out.println(dom("f").value)
    out.println(dom.value)
    out.printExc(dom("val"))
    out.printExc(dom("@val"))
    //check that the DOMPrefixTree behaves well first by printing it then by copying it back into a PrefixTree
    out.println(dom)
    out.println(dom.copy[Int,PrefixTree[String,Int]])
    //check that XPath finds nodes that are DOMPrefixTree
    for (xpath <- findXpath) {
      val r = DOMPrefixTree(dom.find(xpath).item(0))
      r.asXml(out,false)
      out.println
      out.println(r)
    }
  }
  def testDOM1(implicit out:PrintWriter) = {
    //value as text node
    testDOM(out,DOMPrefixTree.Params[Int](javax.xml.parsers.DocumentBuilderFactory.newInstance.newDocumentBuilder.newDocument,(_:Int).toString,null),"d","*[text()=6]")
  }
  def testDOM2(implicit out:PrintWriter) = {
    //value as attribute
    testDOM(out,DOMPrefixTree.Params[Int](javax.xml.parsers.DocumentBuilderFactory.newInstance.newDocumentBuilder.newDocument,(_:Int).toString,"@val"),"d","*[x]")
  }
  def testDOM3(implicit out:PrintWriter) = {
    //value in embedded element
    testDOM(out,DOMPrefixTree.Params[Int](javax.xml.parsers.DocumentBuilderFactory.newInstance.newDocumentBuilder.newDocument,(_:Int).toString,"val"),"d","*[val=5]")
  }
  def testDOM4(implicit out:PrintWriter) = {
    //some leaves as attributes, use of namespace and renaming of some nodes
    testDOM(out,DOMPrefixTree.Params[Int](PrefixTreeLikeBuilder.noElt,true,javax.xml.parsers.DocumentBuilderFactory.newInstance.newDocumentBuilder.newDocument,"_",(_:Int).toString,"@val",(_:String) match {case "a"=>"@x:aa";case "b"=>"@x:bb";case "c"=>"@x:cc";case "f"=>"x:ff";case x=>x},"my0-ns",("my-ns","x")),"my0-ns:d","my-ns:*")
  }

  def main(args:Array[String]):Unit = {
    implicit val out = new PrintWriter(System.out)
    //testFlatMap1
    (new TreeTests).apply(false,true)
    out.flush
  }
}
