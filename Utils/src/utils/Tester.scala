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

/** Used to pass a test.
 *  @param junit, indicates if junit is used, in which case junit methods are used.
 *         otherwise, the result is printed on the console.
 */
class Tester(val junit:Boolean,val handlers:Seq[Tester.Comparator]) {
  val comp = handlers.reduce(_+_)
  def file(name:String) = dir.resolve(new URI(name).getPath)
  val dir = new File(".").toURI.resolve(new URI(s"test/${getClass.getSimpleName}/"))
  val uri = dir.toURL.toURI.toASCIIString
  val fin = file("result")
  val in  = new String(utils.Reader(fin))
  val out = if (junit) new StringWriter else new OutputStreamWriter(System.out)

  @Test def test(runIt: =>Any) = {
    out.write(runIt.toString)
    out.write("\n")
    out.flush
    if (junit) Tester.checkStrings(comp)(in,out.toString) {
      println(s"comparing with $fin")
      val f1 = new File(new URI(fin.toString+".out"))
      println(f1)
      f1.createNewFile()
      val out1 = new FileWriter(f1)
      out1.write(out.toString)
      out1.close
    }
  }
}

object Tester {
  val p1 = Pattern.compile("\\s*")

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
  abstract class Comparator extends ((Int,Int,String,Int,Int,String)=>Option[(Int,Int)]) { self=>
    //combines two handlers to pass them in sequence: the second passes if the first fails and so on.
    def +(c:Comparator):Comparator = new Comparator {
      def apply(start1:Int,idx1:Int,s1:String,start2:Int,idx2:Int,s2:String):Option[(Int,Int)] = {
        self(start1,idx1,s1,start2,idx2,s2) match {
          case None => c(start1,idx1,s1,start2,idx2,s2)
          case x    => x
        }
      }
    }    
  }
  
  abstract class DHandler extends Comparator with ((Int,Int,String)=>Int) { self=>
    //compares two inputs for that handler.
    //returns the next index to use in case of a match.
    def apply(start1:Int,idx1:Int,s1:String,start2:Int,idx2:Int,s2:String):Option[(Int,Int)] = {
      val n1 = self(start1,idx1,s1)
      val n2 = self(start2,idx2,s2)
      if (n1<0 || n2<0) None else Some((n1,n2))
    }
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
    def checkDir(start:Int,idx:Int,s:String) = if (idx==0) -1 else {
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
    var i1=0
    var i2=0
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
    do {
      val c1 = s1.charAt(i1)
      val c2 = s2.charAt(i2)
      if (c1=='\n') { k1=i1+1; l+=1 }
      if (c2=='\n') k2=i2+1
      if      (Character.isWhitespace(c1)) i1+=1
      else if (Character.isWhitespace(c2)) i2+=1
      else if (c1!=c2) {
        val l1 = s1.indexOf('\n', k1) match { case -1=>s1.length case x=>x }
        val l2 = s2.indexOf('\n', k2) match { case -1=>s2.length case x=>x }
        handlers(k1,i1,s1,k2,i2,s2) match {
          case None => onFail; fail(s"lines differ at line $l, column ${i1-k1} (<$c1> and <$c2> differ)\n${s1.subSequence(k1,l1)}\n${s2.subSequence(k2,l2)}")
          case Some(x) => i1=x._1; i2=x._2
        }
      } else {
        i1+=1
        i2+=1
      }
    } while (i1<s1.length && i2<s2.length)
    if      (i1==s1.length) end(k2,i2,s2)
    else if (i2==s2.length) end(k1,i1,s1)
  }  
  
}
