package utils

import java.util.regex.Pattern
import scala.collection.mutable._
import scala.collection.generic.MutableMapFactory
import scala.collection.generic.CanBuildFrom

/**
 * This class defines a Map which accepts regex as keys.
 * A regexed key is recognized by some scheme, as defined by getRegex
 * Performances are similar to those of the underlying Map unless patterns are actually present,
 * in which case keys which fail to match may be slower (all patterns must be tried in turn.) 
 */
trait RegexMap[X] extends MapProxy[String,X] {
  //The list of patterns
  private var patterns:LinkedHashMap[String,Pattern] = null
  /** Getting X for a given key */
  abstract override def get(key:String):Option[X] = {
    super.get(key) match {
      case r:Some[_]              => r
      case None if patterns!=null => for (p <- patterns.find(_._2.matcher(key).matches)) return super.get(p._1); None
      case _                      => None
    }
  }
  /** Putting the FieldDef for a given element */
  abstract override def put(key:String,value:X): Option[X] = {
    val r = super.put(key,value)
    getRegex(key) match {
      case None      =>
      case Some(exp) => if (patterns==null) patterns=new LinkedHashMap[String,Pattern]
                        patterns.put(key,Pattern.compile(exp))
    }
    r
  }
  /** Recovers key as a pattern ; we provide a default implementation whereas '{exp}' uses exp as a Pattern. */
  def getRegex(key:String):Option[String] = {
    val max = key.length-1
    if (max>1 && key.charAt(0)=='{' && key.charAt(max)=='}') Some(key.substring(1,max)) else None
  }
}
object RegexMap {
  /** Factory ; the parameter is used as the backing Map ; it can be non empty. */
  def apply[X](backingMap:Map[String,X]) = new RegexMap[X] {
    val self = backingMap
  }
}
