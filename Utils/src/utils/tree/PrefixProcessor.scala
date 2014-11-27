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

abstract class Processor[K,V,+This](val key:K) {this:This=>
  var value:Option[V] = None
  def onInit(key:K):This
  def onValue(value:V):Unit = this.value=Some(value)
  def onEnd():this.type = this
  def top:This
}

class Stack[K,V] {
  var cur:P1 = _
  var f: ((K, P1)) ⇒ Any = _

  def run:Unit = ()

  class P1(key:K, val top:P1) extends Processor[K,V,P1](key) with Traversable[(K,P1)] with PrefixTraversableOnce[K,V,P1] {
    override def stringPrefix:String = super[PrefixTraversableOnce].stringPrefix
    def onInit(key:K):P1 = new P1(key,this)
    def foreach[U](f: ((K, P1)) ⇒ U): Unit = {
      Stack.this.f = f
      run
    }
  }

  def push(key:K)        = cur = cur.onInit(key)
  def pull(value:V):Unit = cur.onValue(value)
  def pull():Unit        = { val c=cur; cur=cur.top; f((c.key,c.onEnd())) }
}
