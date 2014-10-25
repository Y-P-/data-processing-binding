package utils.tree2

import scala.collection.mutable.LinkedHashMap

object XY {
  
  trait Tree[K,+V,+T<:Tree[K,V,T]] {
    protected[this] type Params <: Tree.Params[K,V,T]
    def p:Params
    def v:Option[V]
    def apply(k:K):T
    protected[this] def newBuilder:TreeBuilder[K,V,T]
  }
  object Tree {
    trait Params[K,+V,+T<:Tree[K,V,T]] {
      def noDefault:K=>T
    }
  }
  
  trait TreeBuilder[K,V,T<:Tree[K,V,T]] {
    type P <: Tree.Params[K,V,T]
    def apply(v:Option[V],tree:scala.collection.Map[K,T])(implicit p:P):T
  }
  object TreeBuilder {
    implicit class X[K,V,T<:Tree[K,V,T],G](val fb:TreeBuilder[K,V,T] { type P=G }) {
      def apply(v:V,tree:scala.collection.Map[K,T])(implicit p:G) = fb(Some(v),tree)
    }
  }
  
  abstract class T2[K,+V] extends Tree[K,V,T2[K,V]] {
    protected[this] type Params <: T2.Params[K,V]
  }
  object T2 {
    class Params[K,+V] extends Tree.Params[K,V,T2[K,V]] {
      def noDefault:K=>T2[K,V] = k => throw new NullPointerException
      def emptyMap: scala.collection.Map[K,V] = LinkedHashMap.empty[K,V]
      def b2 = T2
    }
    object Params{
      implicit def p[K,V] = new Params[K,V]
    }
    def apply[K,V]:TreeBuilder[K,V,T2[K,V]] { type P=T2.Params[K,V] } = new TreeBuilder[K,V,T2[K,V]] {
      type P = T2.Params[K,V]
      def apply(value:Option[V],tree:scala.collection.Map[K,T2[K,V]])(implicit param:P) = new T2[K,V] {
        type Params = P
        val p = param
        val v = value
        def apply(k:K):T2[K,V] = tree(k)
        def newBuilder:TreeBuilder[K,V,T2[K,V]] = p.b2.apply[K,V]
      }
    }
  }
  abstract class T1[+V] extends T2[String,V] with Tree[String,V,T1[V]] {
    protected[this] type Params <: T1.Params[V]
  }
  object T1 {
    class Params[+V] extends T2.Params[String,V] with Tree.Params[String,V,T1[V]] {
      override def noDefault:String=>T1[V] = k => throw new NullPointerException
      override def emptyMap: scala.collection.Map[String,V] = LinkedHashMap.empty[String,V]
      def b1 = T1
    }
    object Params{
      implicit def p[V] = new Params[V]
    }
    def apply[V]:TreeBuilder[String,V,T1[V]] { type P=T1.Params[V] } = new TreeBuilder[String,V,T1[V]] {
      type P = T1.Params[V]
      def apply(value:Option[V],tree:scala.collection.Map[String,T1[V]])(implicit param:P) = new T1[V] {
        type Params = P
        val p = param
        val v = value
        def apply(k:String):T1[V] = tree(k)
        def newBuilder:TreeBuilder[String,V,T1[V]] = p.b1.apply[V]
      }
    }
  }
  
  val p1 = T1[Int](Some(1),null)(T1.Params.p)
  T1[Int](Some(2),Map("1"->p1))
  val p2 = T2[String,Int](None,null)(T2.Params.p)
  T2[String,Int](Some(3),Map("1"->p1,"2"->p2))
  val p3 = T1[Int](1,null)

}