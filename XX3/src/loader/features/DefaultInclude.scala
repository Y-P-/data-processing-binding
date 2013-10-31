package loader.features

import java.net.URI
import java.net.URLDecoder

/** Class used by default to solve includes.
 *  
 */
class DefaultInclude {

}

/*
   * Analyzes an uri to build a parser.
   * The URI is built from a normal URI to reach the appropriate resource ; the query part of that
   * URI is expended with the following parts, separated by ';' (this character is not allowed otherwise):
   * <normal uri>;@xxx;params#....
   * where:
   * o @xxx where xxx is a parser kind. That value will have to match an entry in the map for known parsers.
   * o params is a tree of values, where new layers are within parenthesis, values are separated by ; and association
   *   done with an equal sign. At any point it is allowed to develop a value by separating names by .
   *   e.g. x=1;y=(s=3;t=(u=5,v=6);a.b=8) defines x=1,y.s=3,y.t.u=5,y.t.v=6,y.a.b=8.
   * Both are optional.
   */
  object URIAnalyzer {
    import java.util.regex.Pattern
    import java.io.File
    import java.net.URISyntaxException
    import scala.collection.mutable.HashMap
  
    protected val p = Pattern.compile("(@([^;#]+)?|@([^;#]+);([^#]+)|([^#]+))(#.*)?")
    protected val local = new File(".").toURI
    /*
    /**
     * Analyzes the given URI and returns an URILoader:
     * @param uri, the uri pointing to the data to load, including local settings.
     * @return an URILoaders, that is an engine ready to be used
     */
    @throws(classOf[IllegalArgumentException])
    def apply(uri:URI):URILoaders = try {
      //use regex to analyze the URI query part
      val q  = URLDecoder.decode(uri.getQuery,"UTF-8")
      val m = if (q!=null) p.matcher(q) else throw new IllegalArgumentException(s"<$uri> : URI with query part required : $p\nURI might be misformed")
      if (!m.matches) throw new IllegalArgumentException(s"$uri is misformed ; required : $p")
      val mode   = if (m.group(2)!=null) m.group(2) else m.group(3)
      val params = if (m.group(4)!=null) m.group(4) else m.group(5)
      val end    = m.group(6)
      

      //build the result
      new URILoaders(parserMap(found("mode")),uri1,encoding,pParam,mParam.map(x=>(x._1.substring(2),x._2)),vars,fast,features,recorder)
    } catch {
      case e:IllegalArgumentException => throw e
      case e:Throwable                => throw new IllegalArgumentException(s"parser could not be built : ${e.getMessage}",e)
    }
    
    //finds the next char c. -1 if not found. until excluded.
    protected def find(s:CharSequence,c:Char,from:Int,until:Int):Int = {
      var i=from; if (i<until) do { if (s.charAt(i)==c) return i else i+=1 } while (i<until); -1
    }
    //finds the next closing char c that matches the opening char o. assumes that s(from-1)=o. -1 if not found. until excluded.
    protected def findMatch(s:CharSequence,o:Char,c:Char,from:Int,until:Int):Int = {
      var i=from; var k=1; if (i<until) do { val x=s.charAt(i); if (x==c) k-=1 else if (x==o) k+1; if (k==0) return i; i+=1 } while (i<until); -1
    }
    //returns the param value: (composite,beg,end) ; end excluded, until excluded.
    protected def param(s:CharSequence,o:Char,c:Char,e:Char,sep:Char,from:Int,until:Int):(Boolean,Int,Int) = {
      val n = find(s,e,from,until)
      if (n<0) throw new IllegalStateException(s"$e wad expected in $s")
      if (s.charAt(n+1)=='(') {
        val r = findMatch(s,'(',')',n+2,until)
        if (r<0) throw new IllegalStateException(s"opening $o unmatched in $s")
        (true,n+2,r)
      } else {
        val r=find(s,sep,n+1,until)
        (false,n+1,if (r<0) until else r)
      }
    }
    protected def params(s:CharSequence,o:Char,c:Char,e:Char,sep:Char,from:Int,until:Int) = {
      val a = scala.collection.mutable.ArrayBuffer.empty[(Boolean,Int,Int)]
      var i = from
      do {
        val r = param(s,o,c,e,sep,i,until)
        a += r
        i = r._3
      } while (i<until);
      a
    }*/
}  