package utils

/** This object contains new string contextes for specific uses.
 */
object stringContextes {
  
  implicit class RegexPattern(val pattern:java.util.regex.Pattern) extends AnyVal {
    def unapply(s:String):Boolean = pattern.matcher(s).matches
  }
  
  //use r"" to build the corresponding regex
  implicit class RegexPatternContext(val sc: StringContext) extends AnyVal {
    def r(args:Any*):RegexPattern = new RegexPattern(java.util.regex.Pattern.compile(sc.s(args:_*)))
    def r:RegexPattern = new RegexPattern(java.util.regex.Pattern.compile(sc.parts(0)))
  }
  
  //use /"" to split the string into the sequence of / separated elements
  implicit class RegexSeqContext(val sc: StringContext) extends AnyVal {
    def split(args:Any*):Seq[String] = { val x=sc.s(args:_*); if (x=="/") Seq("") else x.split("/",-1) }
    def split:Seq[String] = split(sc.parts(0))
  }

}