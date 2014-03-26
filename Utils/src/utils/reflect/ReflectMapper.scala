package utils.reflect

import java.lang.reflect.Field
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.collection.Map

object ReflectMapper {
  /**
   * The map containing the definition of all classes already analyzed.
   * The analysis would always yield the same result : better to cache it.
   */
  private val map = new HashMap[Class[_<:AnyRef],scala.collection.Map[String,Field]]
  
  /**
   * Lists all fields for a class, including superclass protected/private ones
   */
  @tailrec protected final def getFields(cz:Class[_],buf:ListBuffer[Field]=new ListBuffer[Field]):ListBuffer[Field] =
    if (cz==classOf[AnyRef]) buf else getFields(cz.getSuperclass,buf ++ cz.getDeclaredFields)

  /** Sets the fields of X to the values found in data through reflexion, or with default if not found. As efficient as reflection allows. */
  def set[X](x:X,data:Map[String,Any],default:X=null.asInstanceOf[X]):X = {
    val cz = x.getClass.asSubclass(classOf[AnyRef])
    //get cz in cache or build it
    val a = map.get(cz) match {
      case None    => val m = getFields(x.getClass).map((f:Field) => { f.setAccessible(true); (f.getName,f) }).toMap
                      map += cz -> m
                      m
      case Some(x) => x
    }
    for (t <- a) data.get(t._1) match {
      case None if (default!=null && default!=x) => t._2.set(x,t._2.get(default))  //set from default
      case Some(v)                               => t._2.set(x,v)                  //set from map
      case _                                     =>                                //no default : do nothing
    }
    x
  }
  
}