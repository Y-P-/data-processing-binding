package utils.tree2

import scala.annotation.tailrec
import scala.concurrent._

/**
 * @param K, the key type ; it is not allowed to be an Option, nor to be null
 * @param V, the value type
 */
class PullAdapter[K<:AnyRef,V] extends PushPull[K,V] {

  /** Used to turn the push/pull into a PrefixTraversableOnce.
   *  The 'once' is not to trifle with: even 'hasNext' cannot be called more than once per element!
   */
  class Layer extends Iterator[(K,Layer)] with PrefixTraversableOnce[K,V,Layer] {
    var next:(K,Layer)  = _
    var value:Option[V] = None
    @tailrec final def hasNext:Boolean = item.take match {
        case PullAdapter.End => false
        case o:Option[V]     => if (value.isDefined) throw new IllegalStateException("the same element cannot receive two values")
                                value = o
                                hasNext
        case k:K             => next = (k,new Layer)
                                true
      }    
  }  
  
  protected val item = new PullAdapter.ItemLock[AnyRef]
  
  final def push(k:K) = item.put(k)
  final def pull(v:V) = item.put(Some(v))
  final def pull      = item.put(PullAdapter.End)
  
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
  
  class ItemLock[E>:Null] {
    import java.util.concurrent.locks.Condition;
    import java.util.concurrent.locks.ReentrantLock;
    var item:E = null
    val lock = new ReentrantLock(false)
    val notEmpty = lock.newCondition
    val notFull =  lock.newCondition
    def take:E = {
      lock.lockInterruptibly
      try {
        while (item == null)
          notEmpty.await
        val x = item
        item = null
        notFull.signal
        x
      } finally {
        lock.unlock
      }
    }
    def put(e:E):Unit = {
      lock.lockInterruptibly
      try {
        while (item != null)
          notFull.await
        item = e;
        notEmpty.signal
      } finally {
        lock.unlock
      }
    }
  }
  
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