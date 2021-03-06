package utils.reflect

import utils.reflect.Converters.FromString
import utils.reflect.Reflect._
import utils.reflect._


/** A class used to invoke the method used to return the correct value upon completion.
 *  For example, upon reading a data object containing year/month/day, you may want to return
 *  not that data structure, but a standard date.
 *  This will be achieved by invoking a 'tagEnd' method, called after building the object.
 *  That method may be:
 *  - either a class method from that object (e.g. the object contains a builtin conversion)
 *    it must not accept any parameter
 *  - or a one parameter static method accepting that kind of object as parameter.
 *  - or a two parameters static method, with the first being compatible with that object
 *    class and the second compatible with the Element class used.
 *  - by default, if nothing is specified, the created object is returned.
 */
class ConversionSolver(
    val stringSolver:(Class[_]=>Option[FromString[_]])                   = Converters.defaultMap,
    val namedSolver:Map[String,Converter[_,_]]                           = null,
    val registeredSolver:Seq[Converter[_,_]]                             = null,
    val collectionSolver:java.lang.reflect.Type=>Option[CollectionAdapter.Adapt] = CollectionAdapter.defaultMap) {
  
  /** Finds an appropriate converter from one of the sources by following these exclusive rules (in order):
   *  - if the name is significant (not null or "")
   *  -   o name starts with @    : take the appropriate entry (e.g. '@xyz') from the named list (no check done: it has to work)
   *  -   o name starts with _.   : check the src then dst class
   *  -   o name starts otherwise : check the class
   *  -   o name ends with .*     : find any appropriate method in the right class (heuristic algorithm based on raw class for src/dst)
   *  -   o name ends otherwise   : the name indicates the method to use for conversion
   *  - if src can be assigned to dst, don't convert
   *  - if src is String, check within default String converters for an appropriate converter (heuristic algorithm based on raw class for dst)
   *  - otherwise check within registered converters for an appropriate converter (heuristic algorithm based on raw class for src and dst)
   *  !!! This works by examining raw types. Generics must not be used!
   */
  def get[U,V](src:Class[U],dst:Class[V],fd:ConvertData,name:String):Either[String,U=>V] = {
    if (name!=null && name.length>0) {
      if (name.charAt(0)=='@') {
        if (namedSolver==null)
          Left(s"no named converters registered: $name cannot be solved")
        else
          namedSolver.get(name).map(_(fd).asInstanceOf[U=>V]).toRight(s"no conversion named $name found")
      } else try {
        val idx    = name.lastIndexOf('.')
        val fName  = name.substring(idx+1) match {
          case "*" => null                 //find any available method
          case  s  => s                    //use that specific method
        }
        val in = name.substring(0,idx) match {
          case "_" => null              //local method in class src or dst
          case  s  => Class.forName(s)  //class for conversion is given
        }
        (if (in==null) Converters(src,src,dst,fName).orElse(Converters(dst,src,dst,fName)) else Converters(in,src,dst,fName)).map(_(fd).asInstanceOf[U=>V]).toRight(s"no Converter from $src => $dst named $name available in either $src or $dst")
      } catch {  //many reasons for failure here!
        case e:Throwable => Left(s"failed to fetch converter $name for $src => $dst: $e")
      }
    } else if (src<dst) {
      //src is subclass of dst ? coerce it and return it
      Right(_.asInstanceOf[V])
    } else if (checkPrimitive(src,dst)) {
      Right(_.asInstanceOf[V])
    } else if (src eq classOf[String]) {
      //src is String ? find a default String conversion to dst
      stringSolver(dst).map(_(fd).asInstanceOf[U=>V]).toRight(s"no default String conversion found for $dst")
    } else if (registeredSolver==null) {
      Left(s"no registered conversion found for $src => $dst")
    } else {
      //otherwise find a registered conversion from src to dst
      var found:Converter[_,_] = null
      for (c <- registeredSolver if c.src>src && c.dst<dst) {
        //take c if dst is at least smaller than the current one and either is not equal or src is smaller and not equal 
        if ((found==null) || (c.dst<found.dst && (!(found.dst<c.dst) || (found.src>c.src && !(c.src<found.src))))) found=c
      }
      if (found==null) Right(found(fd).asInstanceOf[U=>V]) else Left(s"no registered conversion found for $src => $dst")
    }
  }
  
  /** The usual way to find the appropriate conversion from src to dst
   *  @param src,  the source class to convert from
   *  @param dst,  the destination class to convert to
   *  @param name, if provided, only this named conversion is fetched ; otherwise, you can use null
   */
  def apply(src:Class[_],dst:Class[_],fd:ConvertData,name:String):Either[String,Any=>Any] = get[Any,Any](src.asInstanceOf[Class[Any]],dst.asInstanceOf[Class[Any]],fd,name)  
}


  