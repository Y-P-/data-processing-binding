package utils.tree

abstract class PrefixProcessor[-X,R,+P] { this:P=>
  protected[this] type P0<:PrefixProcessor[X,R,P] with P
  
  def onInit(x:X):P0
  def += (result:R):Unit
  def onEnd():R
}

abstract class PrefixTreeProcessor[-X,R,+P] extends PrefixProcessor[X,R,P] { this:P=>
  protected[this] type P0<:PrefixTreeProcessor[X,R,P] with P
  protected[this] def iter(x:X):Iterable[X]
  protected[this] def head:X
  
  def recur:R = {
    val i = iter(head).iterator
    if (i.hasNext) do {
      val r = onInit(i.next)
      if (r!=null) {
        val rr = r.recur
        if (rr!=null) this += rr
      }
    } while (i.hasNext)
    onEnd()
  }
}

class BuilderRec[K,+V,R1<:PrefixTraversableOnce[K,V,R1],R<:PrefixTreeLike[K,V,R]](val head:(K,R1))(implicit bf:PrefixTreeLikeBuilder[K,V,R]) extends PrefixTreeProcessor[(K,R1),(K,R),BuilderRec[K,V,R1,R]] {
  protected[this] type P0 = BuilderRec[K,V,R1,R]
  
  def iter(x:(K,R1)):Iterable[(K,R1)] = x._2.toIterable
  
  def onInit(x:(K,R1)):P0   = new BuilderRec[K,V,R1,R](x)(bf.newEmpty)
  def +=(result:(K,R)):Unit = bf += result
  def onEnd():(K,R)         = (head._1,bf.result(head._2.value,null))
}

class Stack[K,V,R] {
  var cur:Processor[K,V,R] = _
  
  trait Processor[K,V,R] extends PrefixProcessor[K,R,Processor[K,V,R]] {
    protected[this] type P0 = Processor[K,V,R]
    var v:Option[V] = None
    val head:K
    val tail:Processor[K,V,R]
  
    def onInit(key:K):P0
    def += (result:R):Unit
    def onEnd():R
  }
  
  class BR(val head:K,tail:BR)
  
  def push(key:K)        = cur = cur.onInit(key)
  def pull(value:V):Unit = cur.v = Some(value)
  def pull():Unit        = cur.tail += cur.onEnd()
}
