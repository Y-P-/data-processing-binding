package loader


import java.net.{URI,URLEncoder,URLDecoder}
import loader.features._
import loader.reflect.Converter
//import loader.motors.XmlMotor
/*
/**
 * Utility for recovering items form a map with String keys.
 */
trait MapView[X] {
  def apply(name:String):X
}
protected final class ItemMap[X](val self:Map[String,X],val itemClass:String) extends scala.collection.MapProxy[String,X] with MapView[X] {
  //Recovers an item by name
  override def apply(name:String):X = self.get(name) match {
    case Some(x) => x
    case None    => try { Class.forName(name).newInstance().asInstanceOf[X] } catch {
      case e:Throwable => throw new IllegalArgumentException(s"no such $itemClass: <$name> (${e.getMessage})")
    }
  }
}

class ParserFeatures(
  val degenHandler:DegenHandler,         //the handler for degenerated structures (feature)
  val degenListHandler:DegenListHandler, //the handler for degenerated lists (feature)
  val substBuilder:SubstBuilder,         //the variable substitution scheme 
  val isAttr:java.util.regex.Pattern,    //the scheme to identify attributes based on the name for parser that do not natively support attributes
  val errHandler:(Engine)=>ErrorHandler  //the error handler scheme
  ) {
  //used to generate a new set of features using the current features as baseline  
  def apply(values:Map[String,Any]):ParserFeatures = utils.ReflectMapper.set[ParserFeatures](clone.asInstanceOf[ParserFeatures],values)
  def apply(degenHandler:DegenHandler)          = if (degenHandler==null)     this else new ParserFeatures(degenHandler,degenListHandler,substBuilder,isAttr,errHandler)
  def apply(degenListHandler:DegenListHandler)  = if (degenListHandler==null) this else new ParserFeatures(degenHandler,degenListHandler,substBuilder,isAttr,errHandler)
  def apply(substBuilder:SubstBuilder)          = if (substBuilder==null)     this else new ParserFeatures(degenHandler,degenListHandler,substBuilder,isAttr,errHandler)
  def apply(isAttr:java.util.regex.Pattern)     = if (isAttr==null)           this else new ParserFeatures(degenHandler,degenListHandler,substBuilder,isAttr,errHandler)
  def apply(errHandler:(Engine)=>ErrorHandler)  = if (errHandler==null)       this else new ParserFeatures(degenHandler,degenListHandler,substBuilder,isAttr,errHandler)
}

protected abstract class Fact {
  def parserMap:MapView[ParserBuilder]
  def motorMap:MapView[EngineBuilder[_]]
  def degenHandlerMap:MapView[DegenHandler]
  def degenListHandlerMap:MapView[DegenListHandler]
  def substBuilderMap:MapView[SubstBuilder]
  def errHandlerMap:MapView[(Engine)=>ErrorHandler]
  def auditRecorderMap:MapView[Recorder]
  def features:ParserFeatures
  
  def buildFeatures(degenHandler:String=null,degenListHandler:String=null,keyBuilder:String=null,substBuilder:String=null,isAttr:String=null,errorHandler:String=null) = 
    new ParserFeatures(
        if (degenHandler!=null) degenHandlerMap(degenHandler) else features.degenHandler,
        if (degenListHandler!=null) degenListHandlerMap(degenListHandler) else features.degenListHandler,
        if (keyBuilder!=null) keyGeneratorMap(keyBuilder) else features.keyGenerator,
        if (substBuilder!=null) substBuilderMap(substBuilder) else features.substBuilder,
        if (isAttr!=null) if (isAttr.isEmpty) null else java.util.regex.Pattern.compile(isAttr) else features.isAttr,
        if (errorHandler!=null) errHandlerMap(errorHandler) else features.errHandler)
}
*/
/**
 * Factories for creating parsers.
 * The factory contains different defaults values used by a parser and possibly it's spawned sub-parsers (includes.)
 * A parser cannot be directly built : one requires a FactoryBuilder for that, and the Factory.apply method will build
 * a factory, based on the current state of the factory builder.
 */
/*class Factory*//*(
    val parserMap:ItemMap[ParserBuilder],
    val motorMap:ItemMap[EngineBuilder[_]],
    val degenHandlerMap:ItemMap[DegenHandler],
    val degenListHandlerMap:ItemMap[DegenListHandler],
    val substBuilderMap:ItemMap[SubstBuilder],
    val errHandlerMap:ItemMap[(Engine)=>ErrorHandler],
    val auditRecorderMap:ItemMap[Recorder],
    val features:ParserFeatures
    ) extends Fact { self=>
  import Factory._
  implicit private def fact = this
  
  /** A new factory with only features changed. */
  def apply(features:ParserFeatures):Factory = if (features==null) this else {
    val fb = FactoryBuilder(this)
    fb.features=features
    Factory(fb)
  }
  def build(degenHandler:String=null,degenListHandler:String=null,keyBuilder:String=null,substBuilder:String=null,errorHandler:String=null):Factory = 
    apply(buildFeatures(degenHandler,degenListHandler,keyBuilder,substBuilder,errorHandler))
  def apply(degenHandler:DegenHandler=null,degenListHandler:DegenListHandler=null,substBuilder:SubstBuilder=null,errorHandler:(Engine)=>ErrorHandler=null):Factory = 
    apply(features(degenHandler)(degenListHandler)(substBuilder)(errorHandler))
  /**
   * A class to read the information from the URI.
   * 
   * Note that an URI is self contained, as it own the appropriate references
   * to all required info:
   * o source ('file' part of the URI) 
   * o data format (mode parameter)
   * o output requested (motor parameter ; by default, object)
   * o conversion parameters (other parameters)
   * o variables (variable list)
   * 
   * Note that an URI can be partially used, as the built parser and motor
   * are readily available in the result.
   */
  class URILoaders(val ps:ParserSpawner,
                   val uri:URI,
                   val encoding:String,
                   val params:Map[String,String],
                   val motParams:Map[String,String],
                   val vars:Map[String,String],
                   val fast:Boolean,
                   val features:ParserFeatures,
                   val recFct:(Recorder)=>Recorder) {
    protected val pr = new utils.ParamReader(params)
    /** Invokes a child parser for on, using the parameters of this URI loader. */
    def invoke[X](on:Running[Any]):X = {
      val pos  = on.currentPos
      val ctx  = on.ctx.build(vars,recFct,pos,fast)     //using new spawner params (tested), as well as new features (tested)
      val fact = on.factory(features)
      val p    = ps(pr)(fact)
      p.run(ctx,uri,encoding)(on.builder.asInstanceOf[EngineGenerator[X]].apply(on.current.asInstanceOf[Loader],pr))     //overwrite global vars with local ones (tested), use possibly new audit (testd)      
    }
    /** Builds an executor based on the URI info and the passed info. */
    def build(pos:ParserPosition,ctx:UserContext,vars:Map[String,String])(implicit factory:Factory):Runner#Executor = {
      val ctx1 = ctx.build(vars,recFct,pos.asRoot,fast) //using new spawner params (tested), as well as new features (tested)
      val fact = factory(features)
      ps(pr)(fact).run(ctx1,uri,encoding)
   }
  }
  
  /**
   * Analyzes an uri to build a parser.
   * The URI is built from a normal URI to reach the appropriate resource ; the query part of that
   * URI is expended with the following parts, separated by ';' (this character is not allowed otherwise):
   * <normal uri>;mode=xxx;audit=aaa(p1=v1,p2=v2...);q1=w1;q2=w2...;(var1=x1;var2=x2...)#....
   * where:
   * o mode=xxx where xxx is a parser kind recognized by the factory (mandatory)
   * o audit=aaa(p1=v1,p2=v2...)
   *   where aaa is an audit kind recognized by the factory OR nothing (current recorder kept)
   *   p1=v1 etc is the list of parameters for the audit recorder to use
   *   this section is optional:
   *   - nothing :         previous audit used
   *   - audit=-           no audit 
   *   - audit=(...)       previous audit used, with new parameters
   *   - audit=aaa(...)    audit aaa used, with the given parameters
   * o q1=w1 etc (can be repeated for different values) are parameters for the used parser spawner
   * o (var1=x1;var2=x2...) etc are variables for variable substitution
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
    def apply(uri:URI,defaultAuditer:Recorder):URILoaders = try {
      //use regex to analyze the URI query part
      val q  = URLDecoder.decode(uri.getQuery,"UTF-8")
      val m1 = if (q!=null) p1.matcher(q) else throw new IllegalArgumentException(s"<$uri> : URI with query part required : $p1\nURI might be misformed")
      if (!m1.matches) throw new IllegalArgumentException(s"$uri is misformed ; required : $p1")
      val m3 = p3.matcher(if (m1.group(2)!=null) m1.group(2) else m1.group(5))
      if (!m3.matches) throw new IllegalArgumentException(s"$uri is misformed ; query part required : $p3")
      //read the parser/motor params
      val found = new HashMap[String,String]
      for (x <- m3.group(1).split(";").map(_.split("=",2)))
        found.put(x(0), if (x.length>1) x(1) else "true")  //a token that doesn't split is a 'true' toggle
      val recorder:(Recorder)=>Recorder = found.get("audit") match {
        case None    => identity
        case Some(x) => val m2=p2.matcher(x); if (!m2.matches) throw new IllegalArgumentException(s"$uri is misformed ; audit part misformed : $p2")
                        val rData = new HashMap[String,String]
                        val audit = m2.group(1)
                        if (m2.group(2)!=null) for (x <- m2.group(3).split(",").map(_.split("=",2))) rData.put(x(0), if (x.length>1) x(1) else "true")
                        if (audit==null || audit.length==0) (r:Recorder)=>if (r!=null) r(rData) else null
                        else auditRecorderMap(audit) match {
                          case null => (r:Recorder)=>null
                          case ar   => (r:Recorder)=>ar(rData)
                        }                                           
      }
      //read the used variables
      val g      = m3.group(3)
      val param  = if (g!=null) g.split(";") else null
      val vars   = new HashMap[String,String]
      if (param!=null) for (s <- param) { val x = s.split("="); if (x(0).length>0) vars.put(x(0),if (x.length>1) x(1) else "") }
      //read the encoding, xpath and rebuild the URI without all that stuff we used
      val encoding = found.getOrElse("encoding", "ISO-8859-15")
      val fast     = Converter.CvBoolean.read(null,found.getOrElse("fast","true"))
      val uri1     = local.resolve(new URI(uri.getScheme,uri.getAuthority,uri.getPath,m1.group(3),uri.getFragment))
      val (map1,other) = found.partition(_._1.startsWith("f."))
      val features = if (map1.isEmpty) null
                     else              self.features(map1.map { x=>(x._1.substring(2),
                                          x._1 match {
                                            case "f.keyGenerator" => keyGeneratorMap(x._2)  //checked OK
                                            case "f.substBuilder" => substBuilderMap(x._2)
                                            case "f.degenHandler" => degenHandlerMap(x._2)
                                            case "f.errHandler"   => errHandlerMap(x._2)
                                          })
                                       })
      val (mParam,pParam) = other.partition(_._1.startsWith("m."))
      //build the result
      new URILoaders(parserMap(found("mode")),uri1,encoding,pParam,mParam.map(x=>(x._1.substring(2),x._2)),vars,fast,features,recorder)
    } catch {
      case e:IllegalArgumentException => throw e
      case e:Throwable                => throw new IllegalArgumentException(s"parser could not be built : ${e.getMessage}",e)
    }
  
    //mostly for java compat
    @throws(classOf[IllegalArgumentException])
    def apply(uri:String,defaultAuditer:Recorder):URILoaders = try {
      apply(new URI(URLEncoder.encode(uri,"UTF-8")),defaultAuditer)
    } catch {
      case e:URISyntaxException => throw new IllegalArgumentException(e)
    }
    def apply(uri:URI):URILoaders = apply(uri,null)
  }  
}

/**
 * An object that contains system wide default values that cannot be overridden.
 */
object Factory {
  //the built in, original factory, which cannot change unless you do some unsavory casts on the MapViews.
  implicit val builtIn:Factory = null //XXX apply(new FactoryBuilder)
  
  /**
   * Builds a Factory with the parameters of the passed FactoryBuilder.
   * Note that the created factory is immutable and will not therefore not change
   * even if the FactoryBuilder used to create it later changes.
   */
  def apply(fb:FactoryBuilder) = new Factory(
      new ItemMap(fb.ParserMap.toMap,"parser"),
      new ItemMap(fb.MotorMap.toMap,"motor"),
      new ItemMap(fb.DegenHandlerMap.toMap,"degenerated structure handler"),
      new ItemMap(fb.DegenListHandlerMap.toMap,"degenerated list handler"),
      new ItemMap(fb.SubstBuilderMap.toMap,"substitution handler"),
      new ItemMap(fb.ErrHandlerMap.toMap,"error handler"),
      new ItemMap(fb.AuditRecorderMap.toMap,"audit recorder"),
      fb.features)
}

/**
 * This contains global information about mapping text to various elements and default values.
 * It is a class (and not top) because one may want to add or remove/replace or change
 * the name of some items in a specific context.
 * A brand new FactoryBuilder object is not different from the FactoryBuilder singleton,
 * except that it can be modified.
 */
class FactoryBuilder extends Fact {
  import scala.collection.mutable.HashMap
  import loader.parsers._
  import loader.motors._

  /**
   * The list of all known and registered parsing engines.
   * Registering a parsing engine is not mandatory, but not doing so may prevent a correct URI analysis.
   */
  object ParserMap extends HashMap[String,ParserBuilder] {
    put("struct",parsers.Struct)
    //put("sax",SaxParser)
    //put("xml",SaxParser)
    //put("dom",DomParser)
    //put("flat",FlatParser)
    //put("reg",RegistryStringParser)
  }
  /**
   * The list of all known and registered motor engines.
   * Registering a parsing engine is not mandatory, but not doing so may prevent a correct URI analysis.
   */
  object MotorMap extends HashMap[String,EngineBuilder[_]] {
    put("obj",new ObjectMotor(null))
    put("dom",new DomMotor(null))
    put("xml",new XmlMotor(null))
    put("struct",new StructMotor(null))
  }
  
  object DegenHandlerMap extends HashMap[String,DegenHandler] {
    put("fail",failDegenHandler)
    put("conv",convertDegenHandler)
    put("noop",nullDegenHandler)
    put("incl",includeDegenHandler)
    put("term",terminalDegenHandler)
  }
  
  object DegenListHandlerMap extends HashMap[String,DegenListHandler] {
    put("fail",failDegenListHandler)
    put("noop",nullListDegenHandler)
    put("csv",csvDegenListHandler)
    put("csv+",csvTrimmedDegenListHandler)
  }
  
  object SubstBuilderMap extends HashMap[String,SubstBuilder] {
    put("-",noSubstBuilder)
    put("base",varsSubstBuilder)
    put("exec",execSubstBuilder)
    put("full",fullSubstBuilder)
  }
  
  object AuditRecorderMap extends HashMap[String,Recorder] {
    put("-",null)
    put("standard",AuditRecorder())
  }
  
  object ErrHandlerMap extends HashMap[String,(Engine)=>ErrorHandler] {
    put("fail",(x)=>throwAll)
    put("auditOrFail",AuditErrorHandler(_,throwAll))
    put("auditOrPass",AuditErrorHandler(_,handleAll))
    put("pass",(x)=>handleAll)
  }
  
  val parserMap:ItemMap[ParserBuilder]              = new ItemMap(ParserMap.toMap,"parser")
  val motorMap:ItemMap[EngineBuilder[_]]            = new ItemMap(MotorMap.toMap,"motor")
  val degenHandlerMap:ItemMap[DegenHandler]         = new ItemMap(DegenHandlerMap.toMap,"degenerated structure handler")
  val degenListHandlerMap:ItemMap[DegenListHandler] = new ItemMap(DegenListHandlerMap.toMap,"degenerated list handler")
  val substBuilderMap:ItemMap[SubstBuilder]         = new ItemMap(SubstBuilderMap.toMap,"substitution handler")
  val errHandlerMap:ItemMap[(Engine)=>ErrorHandler] = new ItemMap(ErrHandlerMap.toMap,"error handler")
  val auditRecorderMap:ItemMap[Recorder]            = new ItemMap(AuditRecorderMap.toMap,"audit recorder")
  
  var defaultDegenListHandler = "csv+"
  var defaultDegenHandler     = "incl"
  var defaultSubstBuilder     = "full"
  var defaultErrHandler       = "auditOrFail"
  var defaultIsAttr           = ""
  var features = buildFeatures(defaultDegenHandler,defaultDegenListHandler,defaultSubstBuilder,defaultIsAttr,defaultErrHandler)
}

object FactoryBuilder {
  //Rebuilds a FactoryBuilder with the same parameters as the passed factory ; useful for incremental changes.
  def apply(f:Factory):FactoryBuilder = {
    val fb = new FactoryBuilder
    fb.ParserMap.clear
    fb.MotorMap.clear
    fb.DegenHandlerMap.clear
    fb.DegenListHandlerMap.clear
    fb.SubstBuilderMap.clear
    fb.AuditRecorderMap.clear
    fb.ErrHandlerMap.clear
    fb.features = f.features
    fb.ParserMap ++= f.parserMap
    fb.MotorMap ++= f.motorMap
    fb.DegenHandlerMap ++= f.degenHandlerMap
    fb.DegenListHandlerMap ++= f.degenListHandlerMap
    fb.SubstBuilderMap ++= f.substBuilderMap
    fb.AuditRecorderMap ++= f.auditRecorderMap
    fb.ErrHandlerMap ++= f.errHandlerMap
    fb
  }
}*/



