package utils.tree2

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

abstract class PushPull[-K,+V] {
  def push(key:K):Unit
  protected[this] def pull(value:V):Unit
  def pull():Unit
}

object PushPull {
  /*
  class Stack[K,V] extends PushPull[K,V] {
    private var value:Option[V] = None
    private var cur:Loop = _
    class Loop(val k:K,val parent:Loop) extends Iterator[Loop] { self=>
      cur = this
      def value:Option[V] = None
      def next:Loop
      def hasNext:Boolean
      def maybeNext:Future[Loop]
    }
    final def push(k:K) = cur   = new Loop(k,cur)
    final def pull(v:V) = value = Some(v)
    final def pull      = cur   = cur.parent
    def run(x: =>Unit):Loop = new Loop(null.asInstanceOf[K],null) {
        x
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
    }.foreach { null //x =>
     // print(s"${x._1}{${x._2().value}")
    }
  }
  */
}