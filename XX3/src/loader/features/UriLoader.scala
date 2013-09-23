package loader.features

import java.net.{URI,URLEncoder,URLDecoder}
import scala.collection.Map
import loader.reflect.Converter
import loader.core.{ParserSpawner,ParserBuilder}

/**
 * Analyzes an uri to build a parser executor. The URI contains:
 * o source ('file' part of the URI)
 * o data format (mode parameter which determines which parser to use)
 * o parser parameters (for creating the appropriate parser)
 * o otehr parameters (for building the appropriate Top launcher)
 * 
 * The URI is built from a normal URI to reach the appropriate resource ; the query part of that
 * URI is expended with the following parts, separated by ';' (this character is not allowed otherwise):
 * <normal uri>;mode=xxx;q1=w1;q2=w2...;(var1=x1;var2=x2...)#....
 * where:
 * o mode=xxx where xxx is a parser kind recognized by the parserMap (mandatory)
 * o q1=w1 etc (can be repeated for different values) are parameters for the used parser spawner
 *   they should include an encoding (e.g encoding=UTF-8) if the data is not ISO-8859-15 (the default)
 * o (var1=x1;var2=x2...) etc are variables used for building the Top launcher.
 */
object URILoaders {
  import java.util.regex.Pattern
  import java.io.File
  import java.net.URISyntaxException
  import scala.collection.mutable.HashMap

  protected val p1 = Pattern.compile("((mode=.+?)|(.+?)(;(mode=.+?)))(#.*)?")
  protected val p2 = Pattern.compile("(.*?)(\\((.*?)\\))?")
  protected val p3 = Pattern.compile("(.*?)(;\\((.*?)\\))?")
  protected val local = new File(".").toURI

  /**
   * Analyzes the given URI and returns an URILoader:
   * @param uri, the uri pointing to the data to load, including local settings.
   * @return an URILoaders, that is an engine ready to be used
   */
  @throws(classOf[IllegalArgumentException])
  def apply(parserMap:Map[String,ParserSpawner], uri: URI): ParserBuilder#Executor = try {
    //use regex to analyze the URI query part
    val q = URLDecoder.decode(uri.getQuery, "UTF-8")
    val m1 = if (q != null) p1.matcher(q) else throw new IllegalArgumentException(s"<$uri> : URI with query part required : $p1\nURI might be misformed")
    if (!m1.matches) throw new IllegalArgumentException(s"$uri is misformed ; required : $p1")
    val m3 = p3.matcher(if (m1.group(2) != null) m1.group(2) else m1.group(5))
    if (!m3.matches) throw new IllegalArgumentException(s"$uri is misformed ; query part required : $p3")
    //read the parser/motor params
    val found = new HashMap[String, String]
    for (x <- m3.group(1).split(";").map(_.split("=", 2)))
      found.put(x(0), if (x.length > 1) x(1) else "true") //a token that doesn't split is a 'true' toggle
    //read the used variables
    val g = m3.group(3)
    val param = if (g != null) g.split(";") else null
    val vars = new HashMap[String, String]
    if (param != null) for (s <- param) { val x = s.split("="); if (x(0).length > 0) vars.put(x(0), if (x.length > 1) x(1) else "") }
    //read the encoding, xpath and rebuild the URI without all that stuff we used
    val encoding = found.getOrElse("encoding", "ISO-8859-15")
    val fast = Converter.CvBoolean.read(null, found.getOrElse("fast", "true"))
    val uri1 = local.resolve(new URI(uri.getScheme, uri.getAuthority, uri.getPath, m1.group(3), uri.getFragment))
    //build the result
    apply(parserMap(found("mode")), uri1, encoding, found, vars, fast)
  } catch {
    case e: IllegalArgumentException => throw e
    case e: Throwable => throw new IllegalArgumentException(s"parser could not be built : ${e.getMessage}", e)
  }
  
  def apply( ps: ParserSpawner,
             uri: URI,
             encoding: String,
             params: Map[String, String],
             vars: Map[String, String],
             fast: Boolean): ParserBuilder#Executor = ps(params).run(uri,encoding)

}