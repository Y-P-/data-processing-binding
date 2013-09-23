package utils

import java.util.regex.Pattern.compile
import scala.collection.mutable.ArrayBuffer
import java.util.regex.Pattern
import java.util.regex.Matcher

/** This class is designed to manage complex replacements between two regexed strings.
 *  Ex: replacing $1 and $2 inside "abc$1xyz$2uvw$1"
 *      r = new RegexReplacer("abc$1xyz$2uvw$1","\\$[12]",(s:String)=>s(s.length-1)-'1')
 *  
 *  @param pattern is a regex whose groups will be the source of data
 *  @param format  is the string which we will replace
 *  @param marks   is a regex matched inside format
 *  @param index   is a function that provides the input index group from the matched mark
 */
class RegexReplacer[-X](p:Pattern,f:(X,Matcher)=>String) extends ((X,String)=>String) {
  def this(p:String,f:(X,Matcher)=>String) = this(Pattern.compile(p),f)

  def apply(x:X,data:String):String = {
    val m = p.matcher(data)
    var sb:java.lang.StringBuilder = null      //don't build now: there might be no match, hence no copy
    var last = 0
    do {
      val b = m.find()
      if (!b) return if (sb==null) data else sb.append(data, last, data.length).toString
      if (sb==null) sb = new java.lang.StringBuilder //we have a match ? copy required
      sb.append(data, last, m.start)                 //copy unmatched region
      sb.append(f(x,m))                              //copy replacement for matched region
      last = m.end
    } while (true)
    data //never reached
  }
}

object RegexReplacer {
  //Regex replacer based on a map (e.g. """$\(([a-zA-Z0-9_./]+)\)""" for a regex substitution of variable of the kind $(my_name))
  def byMap(regex:String) = new RegexReplacer(regex,(vars:scala.collection.Map[String,String],m:Matcher)=>vars(m.group(1)))
  //Regex replacer based on another regex to select replacements; the source matcher must have matched.
  def byRegex(regex:String,index:(String)=>Int) = new RegexReplacer(regex,(a:Matcher,m:Matcher)=> a.group(index(m.group)))
  //Regex replacer based on a dynamic function of type (X,String)=>String ; a standard regex would be """(.*?)\$\((([a-zA-Z0-9$_.]+):(.*?))\)(.*)""" with the data being in the form $(className:stringparam)
  def byDynamic[X](regex:String,grClzz:Int,grParam:Int) = new RegexReplacer(regex,(x:X,m:Matcher)=>Class.forName(m.group(grClzz)).asSubclass(classOf[(X,String)=>String]).newInstance()(x,m.group(grParam)))
  
  /** Regex replacer based on another regex to select replacements
   *  @param input, the regex for determining input groups
   *  @param regex, the pattern searched
   *  @param index, a function that returns the input group number from a found pattern
   *                e.g. _.last.toInt-'0'.toInt would work well for a pattern like \$[1-9], replacing $1 with the first group etc.
   *  @return a function that lets replace the patterns in y (regex) by the groups matched in x (input).
   */ 
  def byRegex(input:String,regex:String,index:(String)=>Int):(String,String)=>String = {
    val p = Pattern.compile(input)
    val r = RegexReplacer.byRegex(regex,index)
    (x,y) => { val m = p.matcher(x); m.matches; r(m,y) }
  }
  
  /** Test for this class.
   *  java.lang.AssertionError if fails.
   */
  def test():Unit = {
    val x = RegexReplacer.byRegex("""(.*)\.(.*)""","""\$[1-9]""",_.last.toInt-'0'.toInt)
    assert(x("abc.def","$1-$2-$1") == "abc-def-abc")
    val y = RegexReplacer.byDynamic[Int]("""(.*?)\$\((([a-zA-Z0-9$_.]+):(.*?))\)(.*)""",3,4)
    assert(y(3,"$(utils.RegexReplacer$Spawn:coucou)") == "coucou3")
  }
  protected class Spawn extends ((Int,String)=>String) {
    def apply(x:Int,s:String) = s+x
  }
  
}
