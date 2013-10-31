package utils

import scala.collection.mutable.{HashMap,MapProxy}
import utils.Reflect.RichClass

/**
 * A map whose keys are Classes.
 * get reads the exact match or any found super class/interface.
 * If more than one match is found, an exception is thrown.
 * !!! Doesn't support generic types (because they all erase to the same raw class)
 */
class ClassMap[R] extends MapProxy[RichClass[_],R] {
  val self = new HashMap[RichClass[_],R]
  def get(clzz:Class[_]):Option[R] = {
    val cz= new RichClass(clzz)
    self.get(cz) match {
      case None => fetch(cz)
      case e    => e
    }
  }
  protected def fetch(cz:RichClass[_]):Option[R] = {
    var r:(RichClass[_],R) = null
    for (c <- self if cz<c._1.c) {
      if (c._1.isFinal) return Some(c._2)
      if (r==null || cz<r._1) r=c else if (r._2!=c._2) throw new IllegalStateException("more than one possible match for class $cz")
    }
    if (r==null) None else { self.put(cz,r._2); Some(r._2) }
  }
}

object ClassMap {
  def apply[R](x:(Class[_],R)*) = { val map = new ClassMap[R]; map ++= x.map(x=>(new RichClass(x._1),x._2)); map }
}

