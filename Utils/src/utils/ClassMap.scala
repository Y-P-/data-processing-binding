package utils

import utils.Reflect.RichClass
import scala.collection.mutable.HashMap

/** Adds a cache to a function, which prevents recomputing values that are already known.
 *  The A parameter must respect the conditions for being a key in a Map (hashCode, equals.)
 */
trait CachedFunction[-A,+B] extends Function1[A,B] {
  protected[this] val self:A=>B
  private[this] val cache=new HashMap[A,B]
  final def apply(a:A):B = cache.get(a) match {
    case None    => val b=self(a); cache+=((a,b)); b
    case Some(b) => b
  }
}

object ClassMap {
  /** Trait used for creating new entries from a top class. Useful for example for families of classes
   *  that share a common super class such as Java enum or inner classes.
   */
  trait Factory[X,+R] extends (Class[_]=>Option[R]) {
    def max:RichClass[X]
    def build[Y<:X](c:Class[Y]):R
    def apply(c:Class[_]):Option[R] = if (max>c) Some(build(c.asSubclass(max.c))) else None
  }
  
  private[this] class F[R](f:Factory[_,R]*) extends (Class[_]=>Option[R]) {
    def apply(c:Class[_]):Option[R] = { for (f0<-f) { val r=f0(c); if (r!=None) return r }; None }
  }
  
  def apply[R](f:Factory[_,R]*):Class[_]=>Option[R] = new CachedFunction[Class[_],Option[R]] { protected[this] val self=new F(f:_*) }
}

