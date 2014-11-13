package utils.tree2

import scala.annotation.tailrec
import scala.concurrent._

/**
 * @param K, the key type ; it is not allowed to be an Option, nor to be null
 * @param V, the value type
 */
class PullAdapter[K<:AnyRef,V] extends PushPull[K,V] {

  class Layer extends Iterator[(K,Layer)] with PrefixTraversableOnce[K,V,Layer] {
    var next:(K,Layer) = _
    var value:Option[V] = None
    @tailrec final def hasNext:Boolean = queue.take() match {
        case PullAdapter.End => false
        case o:Option[V]     => if (value!=None) throw new IllegalStateException("the same object cannot receive two values")
                                value = o
                                hasNext
        case k:K             => next = (k,new Layer)
                                true
      }    
  }  
  
  val queue = new java.util.concurrent.ArrayBlockingQueue[AnyRef](1)
  
  final def push(k:K) = queue.put(k)
  final def pull(v:V) = queue.put(Some(v))
  final def pull      = queue.put(PullAdapter.End)
  
  def run[U](x: =>Unit)(op: ((K,Layer),Iterator[U]) => U) = {
    import scala.concurrent._
    import java.util.concurrent.Executors
    implicit val executionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    val z = Future { (new Layer).deepForeach[U](null.asInstanceOf[K])(op) }
    x
  }
}

object PullAdapter {
  object End
  
  def main(args:Array[String]):Unit = {
    val s = new PullAdapter[String,String]
    def doIt = {
      s.push("a")
      s.pull
      s.push("aa")
      s.pull("1")
      s.pull
      s.push("ab")
      s.pull("2")
      s.push("aab")
      s.pull("3")
      s.push("d")
      s.pull("5")
      s.pull
      s.pull
      s.push("aac")
      s.pull("4")
      s.pull
      s.pull
      s.pull
    }
    s.run(doIt){ (t,r:Iterator[Unit])=>
      print(s"${t._1}={")
      var i=0
      while (r.hasNext) { i+=1; r.next }
      print(s"}(${if(t._2.value!=None) t._2.value.get else ""})[$i]")
      //print(s"}[$i in ${if (p!=null) p.value.get else "top"}]")
    }
  }
}