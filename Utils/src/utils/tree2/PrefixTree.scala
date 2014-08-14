package utils.tree2

import scala.collection.GenTraversableOnce

/** The standard implementation sits on Maps.
 *  This opens up some opportunities by using Map operations.
 *  Other esoteric implementation could exist, but this one should satisfy most needs, especially
 *  when it can be subclassed while keeping the same subtype for operation return (such as + etc.)
 *  See the StringTree subclass.
 */
class PrefixTree[K,+V](val value:Option[V], val tree: Map[K,PrefixTree[K,V]]) extends AbstractPrefixTreeLike[K,V,PrefixTree[K,V]] {
  protected[this] def newBuilder:PrefixTreeLikeBuilder[K,V,Repr] = PrefixTree.builder[K,V]
  
  def update[L>:K,T>:Repr<:PrefixTreeLike[L,_,T]](kv:(L,T),replace:Boolean): T = kv._2 match {
    case t:PrefixTree[K,V]       => newBuilder(value,tree+((kv._1, if (replace) t else tree.get(kv._1) match { case None=>t; case Some(t1)=>t.update(false,t1) } )))
    case t:PrefixTreeLike[K,_,T] => t.empty//XXX.newBuilder()
  }
  def -(key: K): Repr                    = newBuilder(value,tree-key)
  def get(key: K): Option[Repr]          = tree.get(key)
  def iterator:Iterator[(K, Repr)]       = tree.iterator
  def filterKeys(p: K => Boolean): Repr  = newBuilder(value,tree.filterKeys(p))
  /* overridden for efficiency */
  override def size: Int        = tree.size
  override def isEmpty: Boolean = tree.size == 0
  override def foreach[U](f: ((K,Repr)) => U): Unit = tree.foreach(f)
  
  /** A full map operation that can tranform key, value and Tree type.
   */
  def map[W,L,T<:PrefixTree[L,W] with PrefixTreeLike[L,W,T]](f:V=>W, g:K=>L)(implicit builder:PrefixTreeLikeBuilder[L,W,T]):T = {
    def h(r:Repr):T = builder(r.value.map(f),r.tree.map(x=>(g(x._1),h(x._2))))
    h(this)
  }

  /** A more usual map operation that only tranforms value and Tree type.
   */
  def map[W,T<:PrefixTree[K,W] with PrefixTreeLike[K,W,T]](f:V=>W)(implicit builder:PrefixTreeLikeBuilder[K,W,T]):T = {
    def h(r:Repr):T = builder(r.value.map(f),r.tree.map(x=>(x._1,h(x._2))))
    h(this)
  }
  
  def filterAll(p: ((K,Repr)) => Boolean): Repr  = {
    def h(r:Repr):Repr = newBuilder(r.value,r.tree.filter(p).map(x=>(x._1,h(x._2))))
    h(this)
  }
}

object PrefixTree {
  implicit def builder[K,V] = new PrefixTreeLikeBuilder[K,V,PrefixTree[K,V]](new PrefixTree[K,V](None,Map.empty[K,PrefixTree[K,V]])) {
    def apply(v: Option[V], tree: GenTraversableOnce[(K,PrefixTree[K,V])]):PrefixTree[K,V] = new PrefixTree[K,V](v,empty.tree++tree)
  }
  def apply[K,V](v:Option[V],tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V] = builder(v,tree)
  def apply[K,V](v:V,tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V]         = apply(Some(v),tree)
  def apply[K,V](tree:Map[K,PrefixTree[K,V]]):PrefixTree[K,V]             = apply(None,tree)
  def apply[K,V](v:V):PrefixTree[K,V]                                     = apply(Some(v),Map.empty[K,PrefixTree[K,V]])
}