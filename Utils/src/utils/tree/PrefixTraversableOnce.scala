package utils.tree


/** Describes a tree where data is reached through a succession of keys.
 *  The actual data of type V is optionnal in intermediary nodes, but a well formed tree should not have
 *  empty leaves. Using the methods with (GenTraversable[K],V) parameters (flat view of the tree) rely
 *  on that property (they will omit to output such degenerate branches, and will not build them.)
 *
 *  Operations on trees are not to take lightly ; the recursive nature of trees usually involves that
 *  any transformation be done by rebuilding it. This is even true of the withFilter operation : at this
 *  stage, there is yet no `view` for trees.
 *
 *  The `default` operation usually suggests that it can handle any other value not listed in the
 *  iterable part of the tree. It itself defaults on a generic (but user provided) method.
 *
 *  There are usually three kinds of trees, in ascending order of complexity, memory footprint, building speed:
 *  - Standard trees provide no way to ascend into the tree.
 *  - Weak Navigable trees allow access to the parent, BUT it relies on mutation and may sometimes be
 *    unsafe to use ; in particular, a tree that is built using shared branches may have incorrect parents.
 *    These trees can contain sub-parts that are not navigable.
 *  - Strong Navigable trees are just like weak navigable trees, but nodes are always rebuilt on assignment.
 *    In particular, these tree nodes always correctly point to the right parent. However, the building
 *    cost is usually expensive. Nodes are still mutable on the remove operation where the parent is reset to null.
 */
trait PrefixTraversableOnce[K, +V, +This <: PrefixTraversableOnce[K, V, This]]
  extends TraversableOnce[(K, This)] { self:This =>
  //an alias that concrete classes can use as shortcut to refer to themselves
  protected[this] type Repr = This
  type Key = K
  type Value <: V

  /** The value for the current node */
  def value: Option[V]

  /** true if this is a tree which contains no information (no value, no children, no significant default)
   */
  def isNonSignificant = value.isEmpty && isEmpty

  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   *  You can provide defaults, and error when fetching defaults are ignored (no default in such cases.)
   */
  def asTree[U>:V,R<:PrefixTreeLike[K,U,R],O<:PrefixTreeLike[K,K=>R,O]](default:O with PrefixTreeLike[K,K=>R,O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(cur: Repr,oo:O):R = {
      val b=bf.newEmpty
      for (x <- cur)
        b += ((x._1, recur(x._2,try { oo(x._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] })))
      b.result(cur.value, if(oo==null) null else oo.value.orNull)
    }
    recur(this,default)
  }
  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   */
  def asTree[U>:V,R<:PrefixTreeLike[K,U,R]](implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(cur: Repr):R = {
      val b=bf.newEmpty
      for (x <- cur) b += ((x._1, recur(x._2)))
      b.result(cur.value,null)
    }
    recur(this)
  }

  /** Forces this PrefixTraversableOnce into a PrefixTree with no default.
   */
  def asPrefixTree:PrefixTree[K,V] = asTree[V,PrefixTree[K,V]]

  /** Transform this tree with another tree.
   *  Both trees are explored 'in parallel' and each sub-node of this tree is transformed using the
   *  corresponding sub-node of the transformer tree using the provided `op`.
   *  Default values for the second tree are used but exceptions will end the transformation ungracefully.
   *  The resulting tree contains an appropriate zipped default.
   *  @param t      the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree for defined
   *                values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault is an error or similar answer (e.g. empty)
   *  @param op     an operator that transforms a T node with a This node giving the expected U result
   *  @return the resulting transformed tree
   */
  def zip[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:(T,Repr)=>Option[U])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:Repr):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (!strict || tt.isDefinedAt(x._1)) try { b += ((x._1, recur(tt(x._1),x._2))) } catch { case _:NoSuchElementException => }
      b.result(op(tt,cur),null)
    }
    recur(t,this)
  }
  /** Same, but creates a view.
   */
  def zipView[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:(T,Repr)=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(tt:T,cur:Repr):PrefixTraversableOnce.Abstract[K,U] = new PrefixTraversableOnce.Abstract[K,U](op(tt,cur)) {
      def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = for (x <- cur)
        if (!strict || tt.isDefinedAt(x._1)) try { f((x._1, recur(tt(x._1),x._2))) } catch { case _:NoSuchElementException => }
    }
    recur(t,this)
  }
  /** Same, but creates a view.
   *  Access to all parents of the current element is possible.
   */
  def zipViewRec[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:Seq[(T,Repr)]=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(s:Seq[(T,Repr)]):PrefixTraversableOnce.Abstract[K,U] = {
      new PrefixTraversableOnce.Abstract[K,U](op(s)) {
        def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = {
          val cur = s.head
          for (x <- cur._2)
            if (!strict || cur._1.isDefinedAt(x._1)) try {
              val s1 = (cur._1(x._1),x._2) +: s
              f((x._1, recur(s1)))
            } catch { case _:NoSuchElementException => }
        }
      }
    }
    recur(Seq((t,this)))
  }

  /** Similar to the previous method, but the operation is provided through a third tree which is explored
   *  'in parallel' too.
   *  Default values for both trees are used but exceptions will end the transformation ungracefully.
   *  Note that this operation is one of the most complex for trees, and it could be used to define most other
   *  tree transformations that are defined here, albeit in a more costly way (performance wise.) ; for example
   *  zip above can be expressed here by using a constant op tree ; various map operations can be expressed by
   *  zipping a tree with itself etc...
   *  @param t   the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree or op tree for
   *                defined values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault or O.noDefault is an error or similar answer (e.g. empty)
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node
   *  @return the resulting transformed tree
   */
  def zip2[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(tt:T,cur:Repr,oo:O):R = {
      val b=bf.newEmpty
      for (x <- cur)
        if (strict.succeeds(tt,oo)(x._1)) try { b += ((x._1, recur(tt(x._1),x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
      b.result(oo.value.flatMap(_(tt,cur)),null)
    }
    recur(t,this,op)
  }
  /** Same, but creates a view.
   */
  def zip2View[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(tt:T,cur:Repr,oo:O):PrefixTraversableOnce.Abstract[K,U] = new PrefixTraversableOnce.Abstract[K,U](oo.value.flatMap(_(tt,cur))) {
      def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = for (x <- cur)
        if (strict.succeeds(tt,oo)(x._1)) try { f((x._1, recur(tt(x._1),x._2,oo(x._1)))) } catch { case _:NoSuchElementException => }
    }
    recur(t,this,op)
  }
  /** Same, but creates a view.
   *  Access to all parents of the current element is possible.
   */
  def zip2ViewRec[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,Seq[(T,Repr,O)]=>Option[U],O],R<:PrefixTreeLike[K,U,R]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,Seq[(T,Repr,O)]=>Option[U],O]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(s:Seq[(T,Repr,O)]):PrefixTraversableOnce.Abstract[K,U] = {
      val cur = s.head
      new PrefixTraversableOnce.Abstract[K,U](cur._3.value.flatMap(_(s))) {
        def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = {
          for (x <- cur._2)
            if (strict.succeeds(cur._1,cur._3)(x._1)) try {
              val s1 = (cur._1(x._1),x._2,cur._3(x._1)) +: s
              f((x._1, recur(s1)))
            } catch { case _:NoSuchElementException => }
        }
      }
    }
    recur(Seq((t,this,op)))
  }

  /** Similar to the previous method, but now we can build a tree of a fully different nature (different keys.)
   *  Note that this operation is the most complex for trees and it allows extensive tree transformations.
   *  @param t   the transformer tree
   *  @param strict is true if we don't accept the use of default values for the second tree or op tree for
   *                defined values in the first tree : in that case we fall back on the zipped default : this
   *                usually only makes sense if the T.noDefault or O.noDefault is an error or similar answer (e.g. empty)
   *  @param op  a tree of operators operator that transform the current node using the corresponding transformer node.
   *             an operator produces the new key (if None, the node is skipped), new value, new default handler (can of course be null.)
   *  @return the resulting transformed tree
   */
  def zipFullRec[L,U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[(K,Repr,T,O)])=>(Option[L],Option[U],L=>R),O],R<:PrefixTreeLike[L,U,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[(K,Repr,T,O)])=>(Option[L],Option[U],L=>R),O])(implicit bf:PrefixTreeLikeBuilder[L,U,R]):R = {
    def recur(elt:Seq[(K,Repr,T,O)]):(L,R) = {
      val oo:O = elt.head._4
      val b=bf.newEmpty
      for (x <- elt.head._2)
        if (strict.succeeds(elt.head._3,oo)(x._1)) {
          var t1:T = null.asInstanceOf[T]
          var o1:O = null.asInstanceOf[O]
          if(try {
            //a missing elt (NoSuchElementException) will not produce any result
            t1 = elt.head._3(x._1)
            o1 = oo(x._1)
            true
          } catch { case _:NoSuchElementException => false }) {
            val res = recur((x._1,x._2,t1,o1)+:elt)
            if (res!=null) b += res
          }
        }
      oo.value match {
        case None    => null
        case Some(f) =>
          val res = f(elt)
          res._1 match {
            case Some(l) => (l,b.result(res._2,res._3))
            case _       => null
          }
      }
    }
    recur(Seq((k0,this,t,op)))._2
  }
  /** Same, but creates a view. The default value method goes away in this case.
   */
  def zipFullRecView[L,U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[(K,Repr,T,O)])=>(Option[L],Option[U]),O],R<:PrefixTreeLike[L,U,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[(K,Repr,T,O)])=>(Option[L],Option[U]),O]):PrefixTraversableOnce[L,U,PrefixTraversableOnce.Abstract[L,U]] = {
    def recur(elt:Seq[(K,Repr,T,O)]):(L,PrefixTraversableOnce.Abstract[L,U]) = {
      val oo = elt.head._4
      oo.value match {
        case None    => null
        case Some(f) =>
          val res = f(elt)
          res._1 match {
            case Some(l) => (l,new PrefixTraversableOnce.Abstract[L,U](res._2) {
              def foreach[X](g:((L,PrefixTraversableOnce.Abstract[L,U]))=>X):Unit = for (x <- elt.head._2)
                if (strict.succeeds(elt.head._3,oo)(x._1)) {
                  var t1:T = null.asInstanceOf[T]
                  var o1:O = null.asInstanceOf[O]
                  if(try {
                    //a missing elt (NoSuchElementException) will not produce any result
                    t1 = elt.head._3(x._1)
                    o1 = oo(x._1)
                    true
                  } catch { case _:NoSuchElementException => false }) {
                    val res = recur((x._1,x._2,t1,o1)+:elt)
                    if (res!=null) g(res)
                  }
                }
            })
            case _ => null
          }
      }
    }
    recur(Seq((k0,this,t,op)))._2
  }

  /** A fold left operation that descends through the subtrees.
   *  Children are evaluated before their parent.
   */
  def deepFoldLeft[U](u0:U,k0:K)(f: (U, (K,Repr)) => U): U = {
    def recur(u: U, t:(K,Repr)):U = f(t._2.foldLeft(u)(recur),t)
    recur(u0,(k0,this))
  }

  /** As above.
   *  When processing children, access to the parent is handled down.
   *  @param u0 the initial value
   *  @param k0 a key for the top element
   *  @param f the method used for folding ; it takes three parameters
   *           U the current value
   *           Seq[(K,Repr)] the element stack from bottom to top
   */
  def deepFoldLeft1[U](u0:U,k0:K)(f: (U, Seq[(K,Repr)]) => U): U = {
    def recur(u: U, elt:Seq[(K,Repr)]):U = f(elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt)),elt)
    recur(u0,Seq((k0,this)))
  }
  /** As above.
   *  The operation can change as it is provided through a tree.
   *  Branches for which an operation is missing (NoSuchElementException) are not explored.
   *  Nodes with no value are ignored (but children are explored.)
   */
  def deepFoldLeft2[U,O<:PrefixTreeLike[K,(U, Seq[(K,Repr)]) => U,O]](u0:U,k0:K)(op: O with PrefixTreeLike[K,(U, Seq[(K,Repr)]) => U,O]): U = {
    def recur(u: U, elt:Seq[(K,Repr)], oo:O):U = if (oo==null) u else {
      val u1 = elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt,try { oo(ee._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }))
      oo.value match {
        case Some(g) => g(u1,elt) //has value: compute current element
        case None    => u1        //no value: do nothing
      }
    }
    recur(u0,Seq((k0,this)),op)
  }

  /** A recursive call that descends through the subtrees.
   *  Children are all evaluated within the context of their parent, i.e.
   *  within the 'op' call on their parent.
   *  This produces no result.
   *  @param k an initial key for the top node
   *  @param op, the operation to execute on each node.
   *             (K,Repr) : the current element and its key
   *             =>Unit   : a byname param that has to be evaluated
   *                        somewhere to evaluate the current element children
   */
  def deepForeach(k:K)(op: ((K,Repr),=>Unit) => Unit): Unit = {
    def recur(elt:(K,Repr)):Unit = op(elt,elt._2.foreach(recur))
    recur((k,this))
  }

  /** As above.
   *  Evaluating children produces results that can be used by the parent.
   *  Furthermore, children can be evaluated but don't have to be.
   *  Evaluating children is done by iterating on the provided iterator.
   *  @param k an initial key for the top node ; it is hardly used
   *  @param op, the operation to execute on each node.
   *             (K,Repr)    : the current element and its key
   *             Iterator[U] : the iterator on the children
   *             U           : some result
   */
  def deepForeach1[U](k:K)(op: ((K,Repr),Iterator[U]) => U): U = {
    def recur(elt:(K,Repr)):U = op(elt,elt._2.toIterator.map(recur))
    recur((k,this))
  }

  /** As above.
   *  Children can reach their parent (but not above.)
   *  @param k0 an initial key for the top node ; it is hardly used
   *  @param op, the operation to execute on each node.
   *             Seq[(K,Repr)] : the elements sequence, starting from the current element
   *             Iterator[U]   : the iterator on the children
   *             U             : some result
   */
  def deepForeach2[U](k0:K)(op: (Seq[(K,Repr)],Iterator[U]) => U): U = {
    def recur(elt:Seq[(K,Repr)]):U = op(elt,elt.head._2.toIterator.map(x=>recur(x+:elt)))
    recur(Seq((k0,this)))
  }

  /** As above.
   *  Children can reach their parents.
   *  @param k an initial key for the top node ; it is hardly used
   *  @param op, the operation to execute on each node.
   *             this is a tree and the actual operation can change on each key.
   *             all required key entries must have a matching op.
   *             Repr     : the parent
   *             (K,Repr) : the current element and its key
   *             Iterator : the iterator on the children
   */
  def deepForeach3[O<:PrefixTreeLike[K,(Seq[(K,Repr)],Iterator[Unit]) => Unit,O]](k0:K)(op: O with PrefixTreeLike[K,(Seq[(K,Repr)],Iterator[Unit]) => Unit,O]): Unit = {
    def recur(elt:Seq[(K,Repr)], oo:O):Unit = if (oo!=null && oo.value!=None) {
      oo.value.get(elt,elt.head._2.toIterator.map(x=>recur(x+:elt,try { oo(x._1) } catch { case _:NoSuchElementException => null.asInstanceOf[O] }))) //has value: compute current element
    }
    recur(Seq((k0,this)),op)
  }

  /** Appends all bindings of this tree to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string `end`.
   *  Inside, the string representations of all bindings of this tree.
   *  in the form of `key -> value` are separated by the string `sep`.
   *
   *  @param b     the builder to which strings are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    this.map { case (k, v) => s"$k -> $v" }.addString(b, start, sep, end)

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           Unless overridden in subclasses, the string prefix of every tree is `"Map"`.
   */
  def stringPrefix: String = value match {
    case None    => "Tree"
    case Some(v) => "Tree{"+v+"}"
  }

  override def toString = {
    val b = new StringBuilder
    b.append(stringPrefix)
    addString(b,"(",",",")")
    b.toString
  }

}

object PrefixTraversableOnce {

  abstract class Abstract[K,V](val value:Option[V]) extends Traversable[(K,Abstract[K,V])] with PrefixTraversableOnce[K,V,Abstract[K,V]] {
    this:Abstract[K,V] =>
    override def stringPrefix: String = super[PrefixTraversableOnce].stringPrefix
  }

}

