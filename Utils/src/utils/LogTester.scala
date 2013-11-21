package utils

import java.net.URI
import java.io.{File,OutputStreamWriter,StringWriter,StringReader}
import java.util.regex.Pattern
import java.io.FileWriter
import org.w3c.dom.Element
import org.junit._
import runner.RunWith
import runners.Suite
import Assert._
import java.io.PrintWriter
import scala.annotation.tailrec

/** Used to pass a test.
 *  A file named 'result' is used as a reference base (initialize an empty file at start.)
 *  The result from a test is compared to that file and differences are shown.
 *  It is also written in 'result.out'.
 *  Whenever a run marks a new reference, it is enough to rename the last generated file.
 *  Each test must thus run in a single directory, which usually is fine because that directory
 *  may contain any data specific for that test.
 *  Reaching data files in the test directory is solved by using the file method which is provided.
 *  
 *  @param junit, indicates if junit is used, in which case junit methods are used.
 *         otherwise, the result is printed on the console.
 *  @param handlers, a list of handlers to check for false positive (i.e. expected
 *         differences)
 */
class LogTester(name:String,handlers:LogTester.Comparator*) extends LogTester.Solver {
  val comp = if (handlers.isEmpty) null else handlers.reduce(_+_)
  val dir = new File(".").toURI.resolve(new URI(s"$name"))
  def apply(name:String) = dir.resolve(new URI(name).getPath)
  
  /**
   * The test may produce up to three outputs:
   * - String output, for comparison with a reference
   * - File output, to keep track of the result
   * - Console output, for interactive use
   * When running junit, we don't want console output, but we want file output to be able to analyze
   * the possible differences.
   * When running on the fly, we want all outputs.
   * @param testToDo, the test to do. 
   * @param junit, true if junit is to be used
   * @param dryRun, true if we don't want the file to be created
   */
  def test(testToDo:LogTester.Tester,junit:Boolean,dryRun:Boolean):Unit = {
    val fin  = apply("result")
    val fout = new File(apply("result.out"))
    val str  = new StringWriter
    val out1 = new PrintWriter(str)
    val out2 = new PrintWriter(System.out)
    val out3 = if (dryRun) null else { fout.createNewFile; new PrintWriter(fout) }
    val out  = new PrintWriter(if (junit) tee(out3,out1) else tee(tee(out3,out2),out1))
    try {
      val in = new String(ByteArrayReader(fin))
      val r = testToDo(this,out)
      if (!r.isInstanceOf[Unit]) out.println(r)
      out.flush
      if (junit) LogTester.checkStrings(comp)(in,str.toString) {
        println(s"comparing with $fin")
      }
    } finally {
      out.close
    }
  }
}

object LogTester {
  private[this] val p1 = Pattern.compile("\\s*")
  
  /** used by client code to resolve files in the test directory. name is relative to that directory. */
  trait Solver {
    def apply(name:String):URI
  }
  
  /** a stub for client code testing. Fill up apply using file if necessary. */
  trait Tester { self=>
    def apply(file:Solver,out:PrintWriter):Any
  }
  
  
  /** tester that uses the standard setup */
  trait StandardTester extends Tester { self=>
    val handlers = Seq.empty[LogTester.Comparator]
    /** defines a standard setup: runs occur in test/classname */
    private class Standard extends LogTester("test/"+self.getClass.getSimpleName+"/",handlers:_*)
    //runs the test!
    def apply(junit:Boolean,dryRun:Boolean):Unit = new Standard().test(this,junit,dryRun)
    @Test def test():Unit = apply(true,false)
  }


  /** Defines methods to check to handle expected differences.
   *  This could be for example the values from a random generator, the current time and date, object default printout.
   *  These methods should be selective enough to ensure that only the expected different data is ignored.
   *  The comparison is done line by line, and it is expected that two different runs should yield identical lines.
   *  @param $1, the index for the beginning of the current line (string 1)
   *  @param $2, the index where a character is seen as differing (string 1)
   *  @parma $3, the global string (1) analyzed
   *  @param $4, the index for the beginning of the current line (string 2)
   *  @param $5, the index where a character is seen as differing (string 2)
   *  @parma $6, the global string (2) analyzed
   *  @return the indexes where analysis may resume, None if not match was found
   */
  abstract class Comparator { self=>
    //combines two handlers to pass them in sequence: the second passes if the first fails and so on.
    def +(c:Comparator):Comparator = new Comparator {
      def apply(start1:Int,idx1:Int,s1:String,start2:Int,idx2:Int,s2:String):Option[(Int,Int)] = {
        self(start1,idx1,s1,start2,idx2,s2) match {
          case None => c(start1,idx1,s1,start2,idx2,s2)
          case x    => x
        }
      }
    }
    def apply(start1:Int,idx1:Int,s1:String,start2:Int,idx2:Int,s2:String):Option[(Int,Int)]
  }
  
  abstract class DHandler extends Comparator { self=>
    //compares two inputs for that handler.
    //returns the next index to use in case of a match.
    def apply(start1:Int,idx1:Int,s1:String,start2:Int,idx2:Int,s2:String):Option[(Int,Int)] = {
      val n1 = self(start1,idx1,s1)
      val n2 = self(start2,idx2,s2)
      if (n1<0 || n2<0) None else Some((n1,n2))
    }
    def apply(start:Int,idx:Int,s:String):Int 
  }
  
  //don't compare object addresses! we identify them by their format: @hexsequence
  object checkAddress extends DHandler {
    val p = Pattern.compile("@[0-9a-f]{4,}")
    def apply(start:Int,idx:Int,s:String) = if (idx==0) -1 else {
      var i = idx
      var j = idx
      do { i-=1 } while (i>start && s.charAt(i)!='@')
      do { j+=1 } while ('0'<=s.charAt(j)&&'9'>=s.charAt(j) || 'a'<=s.charAt(j)&&'f'>=s.charAt(j))
      val x = s.subSequence(i,j)
      if (p.matcher(x).matches) j else -1
    }
  }
  //account for relative directory
  object checkUriDir extends DHandler {
    val p = Pattern.compile("((?:.*?)@include:(?:.*?)/test/).*")
    def apply(start:Int,idx:Int,s:String) = if (idx==0) -1 else {
      val x = s.indexOf("?",idx)
      if (x<=0) -1 else {
        val m=p.matcher(s.substring(start,x))
        if (!m.matches) -1 else {
          val r=start+m.group(1).length
          if (r<=idx) -1 else r  //prevents infinite loops in case of a second ? in the query part
        }
      }
    }    
  }
    
  /** This utility compares two strings. If they differ, the onFail method is called and a precise message
   *  highlighting the differences is printed.
   */
  def checkStrings(handlers:Comparator)(s1:String,s2:String)(onFail: =>Unit):Unit = {
    var k1=0
    var k2=0
    var l=0
    def end(start:Int,idx:Int,s:String):Unit = {
      if (p1.matcher(s.subSequence(idx, s.length)).matches) {
        Assert.assertTrue(true)
      } else {
        onFail
        fail(s"lines differ after line $l, column ${idx-start} ; data continues\n${s.subSequence(idx,s.length)}")
      }
    }
    @tailrec def checkSection(idx1:Int,idx2:Int):Unit = {
      if      (idx1==s1.length) end(k2,idx2,s2)
      else if (idx2==s2.length) end(k1,idx1,s1)
      else {
        val c1 = s1.charAt(idx1)
        val c2 = s2.charAt(idx2)
        if (c1=='\n') { k1=idx1+1; l+=1 }
        if (c2=='\n') k2=idx2+1
        if      (Character.isWhitespace(c1)) checkSection(idx1+1,idx2)
        else if (Character.isWhitespace(c2)) checkSection(idx1,idx2+1)
        else if (c1==c2) checkSection(idx1+1,idx2+1)
        else {
          val r = if (handlers!=null) handlers(k1,idx1,s1,k2,idx2,s2) else None
          r match {
            case None => onFail
                         val l1 = s1.indexOf('\n', k1) match { case -1=>s1.length case x=>x }
                         val l2 = s2.indexOf('\n', k2) match { case -1=>s2.length case x=>x }
                         fail(s"lines differ at line $l, column ${idx1-k1} (<$c1> and <$c2> differ)\n${s1.subSequence(k1,l1)}\n${s2.subSequence(k2,l2)}")
            case Some(x) => checkSection(x._1,x._2)
          }
        }
      }
    } 
    checkSection(0,0)
  }  
  
}
