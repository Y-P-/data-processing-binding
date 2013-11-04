package utils

import scala.collection.mutable.{HashMap,MapProxy}
import utils.Reflect.RichClass

/**
 * A map whose keys are Classes.
 * get reads the exact match or any found super class/interface.
 * If more than one match is found, an exception is thrown.
 * !!! Doesn't support generic types (because they all erase to the same raw class)
 */
class ClassMap[R](factory:Seq[ClassMap.Factory[_,R]]) extends MapProxy[RichClass[_],R] {
  val self = new HashMap[RichClass[_],R]
  def get(clzz:Class[_]):Option[R] = {
    val cz= new RichClass(clzz)
    self.get(cz) match {
      case None => fetch(cz)
      case e    => e
    }
  }
  protected def fetch(cz:RichClass[_]):Option[R] = {
    var r:(RichClass[_],Option[R]) = null
    for (f <- factory) {
      f(cz.c) match {
        case None =>
        case x    => if (f.max.isFinal && f.max>cz) return x
                     if (r==null || cz<r._1) r=(f.max,x) else if (r._2!=x) throw new IllegalStateException("more than one possible match for class $cz")
      }
    }
    if (r==null) None else { self.put(cz,r._2.get); r._2 }
  }
}

object ClassMap {
  /** Trait used for creating new entries from a top class. Useful for example for families of classes
   *  that share a common super class such as ava Enum or inner classes.
   */
  trait Factory[X,R] {
    def max:RichClass[X]
    def build[Y<:X](c:Class[Y]):R
    def apply(c:Class[_]):Option[R] = if (max>c) Some(build(c.asSubclass(max.c))) else None
  }
  
  def apply[R](x:(Class[_],R)*)(f:Factory[_,R]*) = { val map = new ClassMap[R](f); map ++= x.map(x=>(new RichClass(x._1),x._2)); map }
}

