package utils.tree

/** This interface is sufficient to create a PrefixTraversableOnce
 *  And even though this is limited in abstraction, it is the minimal
 *  way to represent a tree.
 *  
 *  Whenever you have a new key, you push it.
 *  Whenever you have a value for a key, you pull it.
 *  Whenever you reach the end of a layer, you pull out.
 *  e.g. for this xml: <a><b>1</b>2<c>3</c></a>
 *     push(a)
 *     push(b)
 *     pull(1)
 *     pull
 *     pull(2)
 *     push(c)
 *     pull(3)
 *     pull
 *     pull
 *  
 */
trait PushPull[-K,-V] {
  def push(key:K):Unit
  def pull(value:V):Unit
  def pull():Unit
}


object PushPull {
  import utils.BlockingData
  import scala.annotation.tailrec
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext
  
  /** marker for the PushPull */
  val End = new AnyRef
  
  /** Used to turn the push/pull into a PrefixTraversableOnce.
   *  The 'once' is not to trifle with: even 'hasNext' cannot be called more than once per element!
   *  This implementation is naive as it doesn't care about handling any error.
   *  In particular, the sending thread will lock if a second value is sent to the same item.
   *  XXX improve the naive implementation
   *  
   * @param K, the key type ; it is not allowed to be an Option, nor to be null.
   * @param V, the value type
   * @param item, the blocking buffer
   */
  class Layer[K,V](item:BlockingData[AnyRef]) extends Iterator[(K,Layer[K,V])] with PrefixTraversableOnce[K,V,Layer[K,V]] {
    var next:(K,Layer[K,V])  = _
    var value:Option[V] = None
    @tailrec final def hasNext:Boolean = item.take match {
        case `End`       => false
        case o:Option[V] => if (value.isDefined) throw new IllegalStateException("the same element cannot receive two values")
                            value = o
                            hasNext
        case k:K         => next = (k,new Layer[K,V](item))
                            true
      }    
  }
  
  /** Complement to the previous class to send the push/pull commands.
   * @param K, the key type ; it is not allowed to be an Option, nor to be null
   * @param V, the value type
   */
  class PullAdapter[K<:AnyRef,V] extends PushPull[K,V] {
  
    protected val item = new BlockingData[AnyRef]
    
    /** sends the appropriate command to the imbedded Layer */
    final def push(k:K) = item.put(k)
    final def pull(v:V) = item.put(Some(v))
    final def pull      = item.put(End)
  
    /** converts the push/pull sequence through the given operator. */
    def run[U](op: ((K,Layer[K,V]),Iterator[U]) => U)(implicit executionContext:ExecutionContext):Future[U] =
      Future { (new Layer[K,V](item)).deepForeach1[U](null.asInstanceOf[K])(op) }
  }
  
  /** Creates a pair for creating a PrefixTraversableOnce using a Push/Pull interface */
  def apply[U,K<:AnyRef,V](op: ((K,Layer[K,V]),Iterator[U]) => U)(implicit executionContext:ExecutionContext):(PushPull[K,V],Future[U]) = {
    val p = new PullAdapter[K,V]
    (p, p.run(op))
  }  
}