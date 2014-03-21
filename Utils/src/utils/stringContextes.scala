package utils

/** This object contains new string contextes for specific uses.
 */
object stringContextes {
  
  implicit class RegexPattern(val pattern:java.util.regex.Pattern) extends AnyVal {
    def unapply(s:String):Boolean = pattern.matcher(s).matches
  }
  implicit class RegexContext(val sc: StringContext) extends AnyVal {
    def r(args:Any*):RegexPattern = new RegexPattern(java.util.regex.Pattern.compile(sc.s(args:_*)))
    def r:RegexPattern = new RegexPattern(java.util.regex.Pattern.compile(sc.parts(0)))
  }

}