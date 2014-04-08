package utils

import utils.reflect.Reflect.RichClass
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
  /** Trait used for creating new entries for families of classes
   *  that share a common super class such as Java enum or inner classes.
   */
  trait Factory[X,+R] extends (Class[_]=>Option[R]) {
    def max:RichClass[X]              //the limiting class: anything out yields None
    def build[Y<:X](c:Class[Y]):R     //builds the value for c
    def apply(c:Class[_]):Option[R] = if (max>c) Some(build(c.asSubclass(max.c))) else None
  }
  
  private[this] class F[R](f:Factory[_,R]*) extends (Class[_]=>Option[R]) {
    def apply(c:Class[_]):Option[R] = { for (f0<-f) { val r=f0(c); if (r!=None) return r }; None }
  }
  private[this] class F1[R](f:Factory[_,R]) extends (Class[_]=>Option[R]) {
    def apply(c:Class[_]):Option[R] = f(c)
  }
  private[this] class F2[R](f1:Factory[_,R],f2:Factory[_,R]) extends (Class[_]=>Option[R]) {
    def apply(c:Class[_]):Option[R] = { val r=f1(c); if (r==None) f2(c) else r }
  }
  
  //a factory for building a cached function from multiple Factory.
  def apply[R](f:Factory[_,R]*):Class[_]=>Option[R] = new CachedFunction[Class[_],Option[R]] {
    protected[this] val self= if (f.length==1) new F1(f(0)) else if (f.length==2) new F2(f(0),f(1)) else new F(f:_*)
  }
}

