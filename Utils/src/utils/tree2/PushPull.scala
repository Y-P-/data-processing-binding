package utils.tree2

abstract class PushPull[-K,+V] {
  def push(key:K):Unit
  protected[this] def pull(value:V):Unit
  def pull():Unit
}

object PushPull {
  
  class Stack[K,V] extends PushPull[K,V] {
      var cur:Loop = _
      var f:((K,Loop))=>Any = _
      class Loop extends PrefixTraversableLike[K,V,Loop] { self=>
        protected[this] def newBuilder = ???
        def update1[W >: V, T >: Loop <: PrefixTraversableLike[K,W,T]](kv: (K, T))(implicit bf: PrefixTraversableLikeBuilder[K,W,T]): T = ???
        def foreach[U](f:((K,Loop))=>U) = { Stack.this.f=f; cur=this }
        def value:Option[V] = None
        def push(k:K):Unit = cur = new Inner(k,this)
        def pull(v:V):Unit = () 
        def pull:Loop      = null
      }
      final protected[this] class Inner(k:K,parent:Loop) extends Loop {
        private var value0:Option[V] = None
        override def value          = value0
        override def pull(v:V):Unit = value0 = Some(v) 
        override def pull:Loop      = { println(k); f((k,this)); println("x"+(k,this)); parent }
      }
    final def push(k:K) = cur.push(k)
    final def pull(v:V) = cur.pull(v)
    final def pull      = cur=cur.pull
    def run(x: =>Unit):PrefixTraversableLike[K,V,Loop] = new Loop {
      override def foreach[U](f:((K,Loop))=>U) = {
        super.foreach(f)
        x
      }
    }
  }
  
  def main(args:Array[String]):Unit = {
    val f:Stack[String,String] = new Stack[String,String]
    f.run {
      f.push("a")
      f.pull
      f.push("aa")
      f.pull("1")
      f.pull
      f.pull
    }.foreach { x =>
      println("printing")
      println(s"$x")
    }
  }
  
}