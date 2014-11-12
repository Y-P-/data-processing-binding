package utils.tree2

import scala.annotation.tailrec
import scala.concurrent._

/**
 * @param K, the key type ; it is not allowed to be an Option, nor to be null
 * @param V, the value type
 */
class PullAdapter[K<:AnyRef,V] extends PushPull[K,V] {

  class Layer(val key:K) extends Iterator[Layer] {
    var next:Layer = _
    var value:Option[V] = None
    @tailrec final def hasNext:Boolean = queue.take() match {
        case PullAdapter.End => false
        case o:Option[V]     => if (value!=None) throw new IllegalStateException("the same object cannot receive two values")
                                value = o
                                hasNext
        case k:K             => next = new Layer(k)
                                true
      }
    
    def loop(f: (Layer,()=>Unit)=>Unit) = {
      def recur(l:Layer):Unit = f(l,()=>for (x<-l) recur(x))
      recur(this)
    }
  }  
  
  val queue = new java.util.concurrent.ArrayBlockingQueue[AnyRef](1)
  
  final def push(k:K) = queue.put(k)
  final def pull(v:V) = queue.put(Some(v))
  final def pull      = queue.put(PullAdapter.End)
  
  def run(x: =>Unit)(f:(Layer,()=>Unit)=>Unit) = {
    import scala.concurrent._
    import java.util.concurrent.Executors
    implicit val executionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    val z = Future { new Layer(null.asInstanceOf[K]).loop(f) }
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
    s.run(doIt){ (l,r)=>
      print(l.key+"={");r();if (l.value==None) print("}") else print(s"}(${l.value.get})")
    }
  }
}