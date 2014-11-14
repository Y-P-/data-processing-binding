package utils
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/** This class works as a blocking queue on one item only.
 *  It is slightly more performant than a BlockingQueue because
 *  it has only one element.
 */
class BlockingData[E>:Null] {
  var item:E = null
  val lock = new ReentrantLock(false)
  val notEmpty = lock.newCondition
  val notFull =  lock.newCondition
  
  /** takes the item */
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
  
  /** writes an item */
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