package utils

//utility to read a map
final class ParamReader(val params:scala.collection.Map[String,String]) {
  def isEmpty = params==null || params.isEmpty
  def apply[X](name:String,default:X)(f: (String)=>X):X = params.get(name) match {
    case None    => default
    case Some(x) => f(x)
  }
}
