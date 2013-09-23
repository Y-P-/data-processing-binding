package validator

/**
 * Contains information to both print and analyze intervals.
 */
class Formatter(val leftOpen       :String = "]",
                val rightOpen      :String = "[",
                val internSep      :String = ",",
                val externSep      :String = ",",
                val infEq          :String = "<=",
                val infStrict      :String = "<",
                val supEq          :String = ">=",
                val supStrict      :String = ">",
                val leftInfinite   :String = "-oo",
                val rightInfinite  :String = "+oo",
                val acceptSingleton:Boolean=true,
                val acceptIneq     :Boolean=true
               ) {
  //these are used in the process of analysis, using the previous fields values.
  val (f1,f2,f3,f4) = {
    import java.util.regex.Matcher.{quoteReplacement => qR}
    import java.util.regex.Pattern.{quote => q}
    import java.util.regex.Pattern.compile
    val spP = qR("\\s*")
    val loP = qR(q(leftOpen))
    val roP = qR(q(rightOpen))
    val liP = qR(q(leftInfinite))
    val riP = qR(q(rightInfinite))
    val lim = spP+"("+loP+"|"+roP+")"+spP
    val isP = spP+qR(q(internSep))+spP
    val esP = spP+qR(q(externSep))+spP
    val iEP = qR(q(infEq))
    val iSP = qR(q(infStrict))
    val sEP = qR(q(supEq))
    val sSP = qR(q(supStrict))
    val fmt1x = "@lim@(?:(@li@)|((?:.(?!@is@))*.))@is@(?:(@ri@)|((?:.(?!@lim@))*.))@lim@(@es@.*)?"
    val fmt1 = compile(fmt1x.replaceAll("@li@",liP).replaceAll("@is@",isP).replaceAll("@ri@",riP).replaceAll("@lim@",lim).replaceAll("@es@",esP))
    val fmt2x = "@sp@((@iE@)|(@iS@)|(@sE@)|(@sS@))@sp@(\\S+?)(@es@.*)?"
    val fmt2 = compile(fmt2x.replaceAll("@sp@",spP).replaceAll("@es@",esP).replaceAll("@iE@",iEP).replaceAll("@iS@",iSP).replaceAll("@sE@",sEP).replaceAll("@sS@",sSP))
    val fmt3x = "@sp@(\\S+?)(@es@.*)?"
    val fmt3 = compile(fmt3x.replaceAll("@sp@",spP).replaceAll("@es@",esP))
    val fmt4x = "@es@(.*)"
    val fmt4 = compile(fmt4x.replaceAll("@es@",esP))
    (fmt1,fmt2,fmt3,fmt4)
  }
}
