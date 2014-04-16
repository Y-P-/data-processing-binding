package utils.tests.tree

import utils.tree._
import utils.LogTester._
import java.io.PrintWriter
import org.junit.Test

object TreeTests {

  @Test class TreeTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter) = {
      import out._
      def b(i:Int) = new MapTree[String,Int](Some(i))
      println("=> check creation by using sequences and the Sequence View")
      val m = MapTreeLike(b(0))(
             (Seq("a"),b(1)),
             (Seq("a","b"),b(2)),
             (Seq("a","c"),b(3)),
             (Seq("a","c","f"),b(4)),
             (Seq("a","c","e"),b(5)),
             (Seq("a","c","d"),b(6)),
             (Seq("a","d"),b(7)),
             (Seq("a","d","a"),b(8)),
             (Seq("b","b"),b(9)),
             (Seq("c","c"),b(10)),
             (Seq("c","c","d","x","y"),b(11))
            )
      println(m)
      println("=> check the global iterator by sequence")
      for (x <- new MapTreeLike.SeqTree(m))
        println(s"${x._1} => ${x._2.value}")
      println("=> check standard remove operation")
      m -= Seq("a","c")
      println(m)
      println("=> check that intermediate empty entries are removed in a remove operation")
      m -= Seq("b","b")
      m -= Seq("c","c","d","x","y")
      println(m)
    
      println("=> check that self-referencing trees work ; note: looping is infinite, so we cannot print the tree!")
      import MapTreeLike._
      val m1 = MapTreeLike.withLoop(b(0))(
              (Seq("a"),b(1)),
              (Seq("a","b"),Seq("a","c")),
              (Seq("a","c"),b(3)),
              (Seq("a","c","d"),b(4)),
              (Seq("a","c","e"),Seq())
             )
      val v1 = m1("a","b","d")
      val v2 = m1("a","c","e","a","b","d")
      println(v1.value)
      println(v2.value)
      println(v1 eq v2)  //checks strict equality between objects (i.e. the same trees are referenced)
      println("=> print the tree with loop detection, with top levels last : stressful test on iterators")
      for (x <- m1.seqIterator(false, true))
        println(s"${x._1} => ${x._2.value}")
    }
  }
}
