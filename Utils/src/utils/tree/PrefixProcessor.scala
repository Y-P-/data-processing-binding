package utils.tree

abstract class PrefixProcessor[-X,R,+P] { this:P=>
  protected[this] type P0<:PrefixProcessor[X,R,P] with P
  
  def onInit(x:X):P0
  def += (result:R):Unit
  def onEnd():R
}

abstract class PrefixTreeProcessor[-X,R,+P] extends PrefixProcessor[X,R,P] { this:P=>
  protected[this] type P0<:PrefixTreeProcessor[X,R,P] with P
  protected[this] def i(x:X):Iterable[X]
  protected[this] def head:X
  
  def recur:R = {
    i(head).foreach { x=>
      val r = onInit(x)
      if (r!=null) {
        val rr = r.recur
        if (rr!=null) this += rr
      }      
    }
    onEnd()
  }
}

class BuilderRec[K,+V,R<:PrefixTreeLike[K,V,R],R1<:PrefixTraversableOnce[K,V,R1]](protected[this] val head:(K,R1),val tail:BuilderRec[K,V,R,R1])(implicit bf:PrefixTreeLikeBuilder[K,V,R]) extends PrefixTreeProcessor[(K,R1),(K,R),BuilderRec[K,V,R,R1]] {
  protected[this] type P0 = BuilderRec[K,V,R,R1]
  protected[this] val b=bf.newEmpty
  
  protected[this] def i(x:(K,R1)):Iterable[(K,R1)] = x._2.toIterable
  
  def onInit(x:(K,R1)):P0   = new BuilderRec[K,V,R,R1](x,this)
  def +=(result:(K,R)):Unit = b += result
  def onEnd():(K,R)         = (head._1,b.result(head._2.value,null))
}

class Stack[K,V,R] {
  var cur:UU[K,V,R] = _
  
  trait UU[K,V,R] extends PrefixProcessor[K,R,UU[K,V,R]] {
    protected[this] type P0 = UU[K,V,R]
    var v:Option[V] = None
    val head:K
    val tail:UU[K,V,R]
  
    def onInit(key:K):P0
    def += (result:R):Unit
    def onEnd():R
  }
  
  class BR(val head:K,tail:BR)
  
  def push(key:K)        = cur = cur.onInit(key)
  def pull(value:V):Unit = cur.v = Some(value)
  def pull():Unit        = cur.tail += cur.onEnd()
}
