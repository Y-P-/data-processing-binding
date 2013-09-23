package utils
import scala.annotation.tailrec
import java.util.regex.{Pattern,Matcher}

/**
 * This will substitute the value in vars:
 * e.g. 'xyz' -> 'abac'
 * will change 'hello $(xyz)' to 'hello abac'
 * var names should be alphanum, with _, . and / allowed.
 */
final class MapSubst(vars:scala.collection.Map[String,String]) extends Subst(Pattern.compile("(.*?)\\$\\(([a-zA-Z0-9_./]+)\\)(.*)"),(a:Any,m:Matcher)=>vars(m.group(2)))
/**
 * This will substitute values by dynamically calling a method:
 * e.g. $(toto.X:43,hello,1.23) would invoke toto.X.apply(['43','hello','1.23'])
 * Parameters cannot contain commas (,).
 */
final class ExecSubst[-X] extends Subst(
        Pattern.compile("(.*?)\\$\\((([a-zA-Z0-9$_.]+):(.*?))\\)(.*)"),
        (x:X,m:Matcher)=>Class.forName(m.group(3)).asSubclass(classOf[(X,String)=>String]).newInstance()(x,m.group(4)))


/**
 * A substituter:
 * o Matches sections of the input string on p.
 * o Found matches are replaced by their image through f.
 * @param p, the pattern used
 * @param f, the substitution method
 * @param X, a context class used by f
 * @deprecated("see RegexReplacer)
 */
class Subst[-X](p:Pattern,f:(X,Matcher)=>String) extends ((X,String)=>String) {
  @tailrec final def subst(x:X,done:StringBuilder,remain:String):StringBuilder = { //The recursive way ; uses a StringBuilder and often does useless appends
    val m = p.matcher(remain)
    if (!m.matches) done else subst(x,done.append(m.group(1)).append(f(x,m)),m.group(m.groupCount))
  }  
  final def apply(x:X,data:String):String = { //could do shorter using a recursive method but we want to optimize for the frequent situations with no substitution
    var remain:String      = data
    var done:StringBuilder = null         //not required if no substitution occur
    do {
      val m = p.matcher(remain)
      if (!m.matches) {
        if (done==null)       return remain         //'done' was never built!
        if (remain.length==0) return done.toString
        return done.append(remain).toString
      }
      if (done==null) done=new StringBuilder        //we have a match: 'done' required
      done.append(m.group(1)).append(f(x,m))
      remain = m.group(m.groupCount)
    } while (remain.length!=0)
    done.toString
  }
}

object Subst {
  /** Subst utility to trim double quotes from beginning and end of s */
  def stripQuotes(s:String) = if (s.charAt(0)=='"' && s.charAt(s.length-1)=='"') s.substring(1, s.length-1) else s  
}

