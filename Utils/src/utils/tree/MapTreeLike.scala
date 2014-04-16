package utils.tree

/** This is the mutable version for MapTreeLikeImmutable.
 *  
 *  @param K, the type of Key, which are homogeneous at all levels
 *  @param V, the type of Value, which are homogeneous at all levels
 *  @param This, the upper limit of all contained sub-trees
 */
trait MapTreeLike[K,+V,This<:MapTreeLike[K,V,This]] extends MapTreeLikeImmutable[K,V,This] with scala.collection.mutable.Map[K,This] {self:This=>
  override def apply(key:K) = super[MapTreeLikeImmutable].apply(key)
  override def empty: This = ???     //compiler curiosity: if no implementation, it fails to recognize This as the correct return type

  /** Adds or replaces T for the given sequence of keys */
  def add(keys:Seq[K], tree:This): This = {
    val x = keys(0)
    if (keys.length==1) this += ((x,tree))
    else {
      val t:This = get(x) match {
        case None    => empty
        case Some(m) => m 
      }
      val t1:This = t.add(keys.tail,tree)
      this += ((x,t1))
    }
  }
  
  /** Returns the immutable view for this MapTree.
   *  Beware that only the view is immutable : the underlying MapTree could still mutate if
   *  either you downcast or you simply pass the original class around.
   */
  def immutable:MapTreeLikeImmutable[K,V,This] = this
  
  override def toString = s"${if (value==None) None else value.get}${if (!isEmpty) s" => ${super.toString}" else ""}"
}

object MapTreeLike {
  /** The Tree can conveniently be viewed as a Map with sequences of keys as key.
   *  This is very convenient to build the Tree and iterate through it.
   */
  implicit class SeqTree[K,This<:MapTreeLike[K,_,This]](val self:MapTreeLike[K,_,This] with This) extends scala.collection.mutable.Map[Seq[K],This] {
    def get(keys:Seq[K]):Option[This]      = self.get(keys:_*)
    def +=(kv: (Seq[K], This)): this.type  = { self.add(kv._1,kv._2); this }
    def -=(keys: Seq[K]): this.type        = { self.rem(keys:_*); this }
    def iterator: Iterator[(Seq[K], This)] = self.seqIterator(true,false)
    override def apply(keys: Seq[K]):This  = self.apply(keys:_*)
  }
  
  /** A simple factory.
   */
  def apply[K,This<:MapTreeLike[K,_,This]](empty:MapTreeLike[K,_,This] with This)(seq:(Seq[K],This)*):This = {
    val m = new SeqTree[K,This](empty)
    for (x <- seq) m+=x
    empty
  }
  /** A factory that allows internal references.
   *  Internal references are solved after normal references, in a loop until all are resolved or none can be resolved.
   *  Internal reference resolution doesn't use default values initially.
   *  Beware: a Tree that contains internal references is significantly more complex.
   */
  def withLoop[K,This<:MapTreeLike[K,_,This]](empty:MapTreeLike[K,_,This] with This)(seq:(Seq[K],Either[This,Seq[K]])*):This = {
    val m = new SeqTree[K,This](empty)
    //processes one self-reference not using default
    @inline def check(keys:Seq[K],ref:Seq[K]):Boolean = m.get(keys) match {
                                                    case None    => false
                                                    case Some(v) => m += ((keys, v)); true
                                                }
    //solve all normal entries
    for ((x,Left(v)) <- seq) m += ((x, v))
    //build the list of references to process
    var l = for ((x,Right(s)) <- seq) yield (x,s)
    var ko = false
    do {
      //processes a list of self-reference not using default ; returns the list of unprocessed references
      val l1 = for (r <- l if !check(r._1,r._2)) yield r
      ko = l1.length!=0 && l1.length==l.length
      l = l1
      //loop as long as we solve entries
    } while (l.length>0 && !ko)
    //all was not yet solved: we have to pass through default values now
    if (ko) {
      //use brute force ; a smart policy might attempt to minimize the use of default.
      for (r <- l) m += ((r._1, m(r._2)))
    }
    empty
  }
  
  //implicits that helps using withLoop with an lighter syntax 
  implicit def toLeft[K,This<:MapTreeLikeImmutable[K,_,This]](t:MapTreeLikeImmutable[K,_,This] with This):Either[This,Seq[K]] = Left(t)
  implicit def toRight[K,This<:MapTreeLikeImmutable[K,_,This]](x:Seq[K]):Either[This,Seq[K]] = Right(x)
}