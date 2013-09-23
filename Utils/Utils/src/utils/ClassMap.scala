package utils

import scala.collection.mutable.HashMap

/**
 * A map whose keys are Classes.
 * get reads the exact match or any found super class/interface.
 * There is no way to control the choice in case more than one superclass/interface matches.
 */
class ClassMap[R] extends HashMap[Class[_],R]{
  override def get(clzz:Class[_]) = {
    super.get(clzz) match {
      case e:Some[_] => e
      case None      => fetch(clzz)
    }
  }
  protected def fetch(clzz:Class[_]) = {
    var r:(Class[_],R) = null
    for (c <- this)
      if (c._1.isAssignableFrom(clzz) && (r==null || r._1.isAssignableFrom(c._1)))
        r=c
    if (r==null) None else { put(clzz,r._2); Some(r._2) }
  }
}

object ClassMap {
  def apply[R](x:(Class[_],R)*) = { val map = new ClassMap[R]; map ++= x; map }
}

