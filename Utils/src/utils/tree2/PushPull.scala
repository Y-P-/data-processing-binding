package utils.tree2

abstract class PushPull[-K,+V] {
  def push(key:K):Unit
  protected[this] def pull(value:V):Unit
  def pull():Unit
}

object PushPull {
  
  class Stack[K,V] extends PushPull[K,V] with PrefixTraversableLike[K,V,Stack[K,V]] {
    protected[this] var cur:Import = new Import
    protected[this] def newBuilder: PrefixTraversableLikeBuilder[K,V,Stack[K,V]] = ???
    def update1[W >: V, T >: PushPull.Stack[K,V] <: PrefixTraversableLike[K,W,T]](kv: (K, T))(implicit bf: PrefixTraversableLikeBuilder[K,W,T]): T = ???
    def value: Option[V] = cur.value
    def foreach[U](f: ((K, Stack[K,V])) => U): Unit = ???
    protected[this] class Import extends PushPull[K,V] { self=>
      //val b = bf.newEmpty
      var value:Option[V] = None
      final def push(k:K):Unit = cur   = new InnerImport(k)
      final def pull(v:V):Unit = value = Some(v) 
      def pull = ()
      final protected[this] class InnerImport(k:K) extends Import {
        override def pull = {
          //self.b += ((k,b.result(value)))
          cur=self
        }
      }
    }
    final def                 push(k:K) = cur.push(k)
    final protected[this] def pull(v:V) = cur.pull(v)
    final def                 pull      = cur.pull
    //final def result                    = cur.b.result(cur.value)
  }
  
  def main(args:Array[String]):Unit = {
    val f:Stack[String,String] = new Stack[String,String] {
      push("a")
      push("aa")
      pull("1")
      pull
      pull
    }
    println(f)
  }
  
}