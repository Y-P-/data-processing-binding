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
trait PushPull[-K,-V] {self=>
  def push(key:K):Unit
  def pull(value:V):Unit
  def pull():Unit

  def mapKey[L](f:L=>K):PushPull[L,V] = new PushPull[L,V] {
    def push(key:L):Unit   = self.push(f(key))
    def pull(value:V):Unit = self.pull(value)
    def pull():Unit        = self.pull()
  }

  def mapValue[W](f:W=>V):PushPull[K,W] = new PushPull[K,W] {
    def push(key:K):Unit   = self.push(key)
    def pull(value:W):Unit = self.pull(f(value))
    def pull():Unit        = self.pull()
  }
}


object P {

  def apply[K,V](p:PushPull[K,V]):PrefixTraversableOnce[K,V,_] = {
    null
  }

  //def apply[K,V](g:O[K,V]=>Unit):PrefixTraversableOnce[K,V,PrefixTraversableOnce[K,V,T] forSome { type T<:PrefixTraversableOnce[K,V,T] }] = new O[K,V](g).init

  class O[K,V] {
    var g:this.type=>Unit = _
    var cur:(K,P) = _
    var f1:((K,P))=>Any = _
    class P(key:K,val prev:(K,P)) extends Iterator[(K,P)] with PrefixTraversableOnce[K,V,P] {
      cur = (key,this)
      var value:Option[V] = None
      def next:(K,P)
      def hasNext:Boolean
      override def stringPrefix = super[PrefixTraversableOnce].stringPrefix
    }
    def push(key:K):Unit   = new P(key,cur)
    def pull(value:V):Unit = cur._2.value=Some(value)
    def pull():Unit        = { val c=cur; cur=cur._2.prev; f1(c) }
    def init(g0:this.type=>Unit) = { g=g0; new P(null.asInstanceOf[K],null) }
  }

  def main(args:Array[String]):Unit = {
    val o = new O[String,String]
    val p = o.init { s=>
    s.push("a")
    s.push("b")
    s.pull("2")
    s.pull
    s.pull("1")
    s.pull
    }
    p.deepForeach(null) { (x,r)=>
      println(s"${x._1}={")
      r
      println("}")
    }
  }

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
  class Layer[K,V](p:PushPull[K,V]) extends Traversable[(K,Layer[K,V])] with PrefixTraversableOnce[K,V,Layer[K,V]] {
    protected var value0:Option[V] = None
    def value = value0
    def foreach[U](f: ((K, utils.tree.P.Layer[K,V])) ⇒ U):Unit = {

    }
    override def stringPrefix = super[PrefixTraversableOnce].stringPrefix
  }

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
    protected var next0:(K,Layer[K,V])  = _
    protected var value0:Option[V] = None
    def value = value0
    def next  = next0
    @tailrec final def hasNext:Boolean = item.take match {
        case `End`       => false
        case o:Option[V] => if (value.isDefined) throw new IllegalStateException("the same element cannot receive two values")
                            value0 = o
                            hasNext
        case k:K         => next0 = (k,new Layer[K,V](item))
                            true
      }
  }

  /** Complement to the previous class to send the push/pull commands.
   * @param K, the key type ; it is not allowed to be an Option, nor to be null
   * @param V, the value type
   */
  class PullAdapter[K<:AnyRef,V] extends PushPull[K,V] {

    protected val item = new BlockingData[AnyRef]

    /** sends the appropriate command to the embedded Layer */
    final def push(k:K) = item.put(k)
    final def pull(v:V) = item.put(Some(v))
    final def pull      = item.put(End)

    /** converts the push/pull sequence through the given operator. */
    def run[U](op: ((K,Layer[K,V]),=>Unit) => U)(implicit executionContext:ExecutionContext):Future[Unit] =
      Future { (new Layer[K,V](item)).deepForeach(null.asInstanceOf[K])(op) }
  }

  /** Creates a pair for creating a PrefixTraversableOnce using a Push/Pull interface */
  def apply[U,K<:AnyRef,V](op: ((K,Layer[K,V]),=>Unit) => U)(implicit executionContext:ExecutionContext):(PushPull[K,V],Future[Unit]) = {
    val p = new PullAdapter[K,V]
    (p, p.run(op))
  }
}