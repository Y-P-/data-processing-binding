package utils.tree


/** Describes a tree precursor where data is reached through an iterable (key,sub-tree).
 *  As such, this structure presents a numerous number of interesting properties, the least of which
 *  being that this representation can also be used as views to actual PrefixTree implementations.
 *  Thus, when some complex operation on a tree is undertaken, and one doesn't want to build intermediate
 *  trees, lifting a real Tree to this representation will gain some benefits.
 *
 *  Methods are provided to go both ways between PrefixTreeLike and PrefixTraversableOnce.
 *
 *  For more information about PrefixTreeLike, se that class description which contains additional information.
 *
 *  Methods in this class often come by three:
 *
 *  method        => does something and return an actual Tree structure (usually through an implicit builder)
 *                   it is best used when one requires an actual tree as result
 *  methodView    => as method, but returns a view, i.e. PrefixTraversableOnce
 *                   it is best used when the result is going to be passed to some other tree transforming method
 *  methodViewRec => as mehodView, but in addition the functions passed as parameter can reach up to the top
 *                   of the (key,subtree) recursion, getting information about the parents
 *                   it is obviously more expensive and complex as the other methods and should only be used
 *                   when necessary. No version returning an actual tree is provided as this is not thought to
 *                   bring any significant gain.
 *
 *  These methods often take a transforming function as parameter.
 *  When this function returns a Tuple or an Option, then as a general contract, it is also allowed to return
 *  null: this indicates that the element (and subtree) is not to be processed and this can serve as an imbedded
 *  filter method : using filter is very often useless (and indeed more costly) when used in conjunction with
 *  such methods.
 *
 *  Methods:
 *  - filter remove elements (and their subtree) from the result
 *  - map transforms a tree into another tree using some transformation way
 *  - zip transform a tree into another tree by using at least one other tree ran in parallel
 *  - deepForeach runs through the tree elements in depth, in the tree order, but beginning at the bottom
 *    hence, the last elemen processed is usually the top tree itself
 *  - deepFoldLeft is similar to foldleft in other collections, but it runs through all elements in the
 *    same way as described in deepForeach
 *
 *  In many cases, the transformation methods can themselves be provided as trees, which are also ran through
 *  in parallel with the processed tree. This paves the way to some very complex tree transformations.
 *
 *  Also note that for regularity, most methods require that you pass a "virtual" key for the top tree (which
 *  by definition is not assumed to be a sub-tree and not attached with a key); such a key may or may not be
 *  significant depending on your own provided transformation methods : the methods here have no use with it.
 */
trait PrefixTraversableOnce[K, +V, +This <: PrefixTraversableOnce[K, V, This]]
  extends TraversableOnce[(K, This)] { self:This =>
  import PrefixTraversableOnce._
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
  def toTree[U>:V,R<:PrefixTreeLike[K,U,R],O<:PrefixTreeLike[K,K=>R,O]](default:O with PrefixTreeLike[K,K=>R,O])(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),O)
    val values = (s:Data) => (s._1._2.value,try { if (s._2==null) null else s._2(s._1._1).value.orNull } catch { case _:NoSuchElementException => null })
    val next   = (child:(K,This),s:Data) => s._2(child._1)
    PrefixTraversableOnce[O,K,V,U,R,This](values,next).apply(null.asInstanceOf[K],default,this)
  }
  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   */
  def toTree[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    PrefixTraversableOnce[K,V,U,R,This]((x:(K,This))=>(x._2.value,default(x))).apply(null.asInstanceOf[K],this)
  }
  /** Forces this PrefixTraversableOnce into some PrefixTreeLike representation.
   */
  def toTreeRec[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(default:(Seq[(K,This)])=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    PrefixTraversableOnce[K,V,U,R,This]((x:Seq[(K,This)])=>(x.head._2.value,default(x))).apply(null.asInstanceOf[K],this)
  }

  def zipi0[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O],R<:PrefixTreeLike[K,U,R]](k0:K,t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O],default0:(((K,This),(T,O)))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    type Data = ((K,This),(T,O))
    val values = (cur:Data) => (cur._2._2.value.flatMap(_(cur._2._1,cur._1._2)),default0(cur))
    val next   = (child:(K,This),s:Data) => try {
        val v1 = s._2._1
        val v2 = s._2._2
        if (strict.succeeds(v1,v2)(child._1)) (v1(child._1),v2(child._1)) else null
      } catch { case _:NoSuchElementException => null }
    PrefixTraversableOnce[(T,O),K,V,U,R,This](values,next).apply(k0,(t,op),this)
  }
  /** Forces this PrefixTraversableOnce into a PrefixTree with no default.
   */
  def toPrefixTree:PrefixTree[K,V] = toTree[V,PrefixTree[K,V]](null.asInstanceOf[K])(x=>null)

  /** Forces this PrefixTraversableOnce into a PrefixTree with some default.
   */
  def toPrefixTree[U>:V,O<:PrefixTreeLike[K,K=>PrefixTree[K,U],O]](default:O with PrefixTreeLike[K,K=>PrefixTree[K,U],O]):PrefixTree[K,U] = toTree[U,PrefixTree[K,U],O](default)


  /** filter this tree according to some criteria.
   */
  def filter[U>:V,R<:PrefixTreeLike[K,U,R]](k0:K)(f:((K,This))=> Boolean, default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(cur:Repr,defa:K=>R):R = {
      val b = bf.newEmpty
      for (x <- cur) if (f(x)) b += ((x._1, recur(x._2,default(x))))
      b.result(cur.value,defa)
    }
    recur(this,default((k0,this)))
  }

  /** Complex filter operation acting both on key and value including the current element parents.
   *  A slightly faster implementation might be done directly.
   */
  def filterViewRec(k0:K)(f:(Seq[(K,Repr)])=>Boolean):PrefixTraversableOnce[K,V,PrefixTraversableOnce.Abstract[K,V]] =
    mapViewRec[K,V](k0)(s => if (f(s)) (s.head._1,s.head._2.value) else null)

  /** Simple map for the value element.
   */
  def map[U,R<:PrefixTreeLike[K,U,R]](k0:K)(f:V=>U,default:((K,This))=> K=>R)(implicit bf:PrefixTreeLikeBuilder[K,U,R]):R = {
    def recur(cur:Repr,defa:K=>R):R = {
      val b = bf.newEmpty
      for (x <- cur) b += ((x._1, recur(x._2,default(x))))
      b.result(if (cur.value==None) None else Some(f(cur.value.get)),defa)
    }
    recur(this,default(k0,this))
  }

  /** Simple map for the value element.
   */
  def mapView[U](f:V=>U):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(cur:Repr):PrefixTraversableOnce.Abstract[K,U] = new PrefixTraversableOnce.Abstract[K,U](cur.value.map(f)) {
      def foreach[X](f0:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = for (x <- cur)
        f0((x._1, recur(x._2)))
    }
    recur(this)
  }

  /** Complex map/filter operation acting both on key and value.
   *  The map operation may refer to any of the current element parent key or element.
   *  It returns a (L,Option[U]) pair, which can be null: such null values are filtered out (as are their children.)
   */
  def mapViewRec[L,U](k0:K)(f:(Seq[(K,Repr)])=>(L,Option[U])):PrefixTraversableOnce[L,U,PrefixTraversableOnce.Abstract[L,U]] = {
    def recur(cur:Seq[(K,Repr)]):(L,PrefixTraversableOnce.Abstract[L,U]) = f(cur) match {
      case null => null
      case zz   => (zz._1,new PrefixTraversableOnce.Abstract[L,U](zz._2) {
        def foreach[X](f0:((L,PrefixTraversableOnce.Abstract[L,U]))=>X):Unit = for (x <- cur.head._2) {
          val r1 = recur(x+:cur)
          if (r1!=null) f0(r1)
        }
      })
    }
    recur(Seq((k0,this))) match {
      case null  => PrefixTraversableOnce.empty
      case (_,r) => r
    }
  }

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
    def recur(tt:T,cur:Repr):R = op(tt,cur) match {
      case null => null.asInstanceOf[R]
      case u    => val b=bf.newEmpty
        for (x <- cur)
          if (!strict || tt.isDefinedAt(x._1)) try {
            val r1 = recur(tt(x._1),x._2)
            if (r1!=null) b += ((x._1, r1))
          } catch { case _:NoSuchElementException => }
        b.result(u,null)
    }
    recur(t,this) match {
      case null => bf.empty
      case r    => r
    }
  }
  /** Same, but creates a view.
   */
  def zipView[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:(T,Repr)=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(tt:T,cur:Repr):PrefixTraversableOnce.Abstract[K,U] = op(tt,cur) match {
      case null => null
      case u    => new PrefixTraversableOnce.Abstract[K,U](u) {
        def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = for (x <- cur)
          if (!strict || tt.isDefinedAt(x._1)) try {
            val r1 = recur(tt(x._1),x._2)
            if (r1!=null) f((x._1,r1))
          } catch { case _:NoSuchElementException => }
      }
    }
    recur(t,this) match {
      case null => PrefixTraversableOnce.empty
      case r    => r
    }
  }
  /** Same, but creates a view.
   *  Access to all parents of the current element is possible.
   */
  def zipViewRec[U,T<:PrefixTreeLike[K,_,T],R<:PrefixTreeLike[K,U,R]](t:T,strict:Boolean,op:Seq[(T,Repr)]=>Option[U]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    def recur(s:Seq[(T,Repr)]):PrefixTraversableOnce.Abstract[K,U] = op(s) match {
      case null => null
      case u    => new PrefixTraversableOnce.Abstract[K,U](u) {
        def foreach[X](f:((K,PrefixTraversableOnce.Abstract[K,U]))=>X):Unit = {
          val cur = s.head
          for (x <- cur._2)
            if (!strict || cur._1.isDefinedAt(x._1)) try {
              val r1 = recur((cur._1(x._1),x._2) +: s)
              if (r1!=null) f((x._1, r1))
            } catch { case _:NoSuchElementException => }
        }
      }
    }
    recur(Seq((t,this))) match {
      case null => PrefixTraversableOnce.empty
      case r    => r
    }
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
    type Data = ((K,This),(T,O))
    val values = (cur:Data) => (cur._2._2.value.flatMap(_(cur._2._1,cur._1._2)), null)
    val next   = (child:(K,This),s:Data) => try {
        val v1 = s._2._1
        val v2 = s._2._2
        if (strict.succeeds(v1,v2)(child._1)) (v1(child._1),v2(child._1)) else null
      } catch { case _:NoSuchElementException => null }
    PrefixTraversableOnce[(T,O),K,V,U,R,This](values,next).apply(null.asInstanceOf[K],(t,op),this)
  }
  /** Same, but creates a view.
   */
  def zip2View[U,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(T,Repr)=>Option[U],O]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,(T,Repr)=>Option[U],O]):PrefixTraversableOnce[K,U,PrefixTraversableOnce.Abstract[K,U]] = {
    type Data = ((K,This),(T,O))
    val values = (cur:Data) => cur._2._2.value.flatMap(_(cur._2._1,cur._1._2))
    val next   = (child:(K,This),s:Data) => try {
        val v1 = s._2._1
        val v2 = s._2._2
        if (strict.succeeds(v1,v2)(child._1)) (v1(child._1),v2(child._1)) else null
      } catch { case _:NoSuchElementException => null }
    PrefixTraversableOnce[(T,O),K,V,U,This](values,next).apply(null.asInstanceOf[K],(t,op),this)
  }
  /** Same, but creates a view.
   *  Access to all parents of the current element is possible.
   */
  def zip2ViewRec[W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,Seq[((K,This),(T,O))]=>Option[W],O]](t:T,strict:Strictness,op:O with PrefixTreeLike[K,Seq[((K,This),(T,O))]=>Option[W],O]):PrefixTraversableOnce[K,W,PrefixTraversableOnce.Abstract[K,W]] = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data) => cur.head._2._2.value.flatMap(_(cur))
    val next   = (child:(K,This),s:Data) => try {
        val v1 = s.head._2._1
        val v2 = s.head._2._2
        if (strict.succeeds(v1,v2)(child._1)) (v1(child._1),v2(child._1)) else null
      } catch { case _:NoSuchElementException => null }
    PrefixTraversableOnce[(T,O),K,V,W,This](values,next).apply(null.asInstanceOf[K],(t,op),this)
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
  def zipFullRec[L,W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W],L=>R),O],R<:PrefixTreeLike[L,W,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W],L=>R),O])(implicit bf:PrefixTreeLikeBuilder[L,W,R]):R = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data) => cur.head._2._2.value.map(_(cur)).orNull
    val next   = (child:(K,This),s:Data) => try {
        val v1 = s.head._2._1
        val v2 = s.head._2._2
        if (strict.succeeds(v1,v2)(child._1)) (v1(child._1),v2(child._1)) else null
      } catch { case _:NoSuchElementException => null }
    PrefixTraversableOnce[(T,O),K,V,L,W,R,This](values,next).apply(k0,(t,op),this)
  }
  /** Same, but creates a view. The default value method goes away in this case.
   */
  def zipFullRecView[L,W,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,(Seq[((K,This),(T,O))])=>(L,Option[W]),O],R<:PrefixTreeLike[L,W,R]](k0:K,strict:Strictness,t:T,op:O with PrefixTreeLike[K,Seq[((K,This),(T,O))]=>(L,Option[W]),O]):PrefixTraversableOnce[L,W,PrefixTraversableOnce.Abstract[L,W]] = {
    type Data = Seq[((K,This),(T,O))]
    val values = (cur:Data) => cur.head._2._2.value.map(_(cur)).orNull
    val next   = (child:(K,This),s:Data) => try {
        val v1 = s.head._2._1
        val v2 = s.head._2._2
        if (strict.succeeds(v1,v2)(child._1)) (v1(child._1),v2(child._1)) else null
      } catch { case _:NoSuchElementException => null }
    PrefixTraversableOnce[(T,O),K,V,L,W,This](values,next).apply(k0,(t,op),this)
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
  def deepFoldLeftRec[U](u0:U,k0:K)(f: (U, Seq[(K,Repr)]) => U): U = {
    def recur(u: U, elt:Seq[(K,Repr)]):U = f(elt.head._2.foldLeft(u)((uu,ee)=>recur(uu,ee+:elt)),elt)
    recur(u0,Seq((k0,this)))
  }
  /** As above.
   *  The operation can change as it is provided through a tree.
   *  Branches for which an operation is missing (NoSuchElementException) are not explored.
   *  Nodes with no value are ignored (but children are explored.)
   */
  def deepFoldLeftRec[U,O<:PrefixTreeLike[K,(U, Seq[(K,Repr)]) => U,O]](u0:U,k0:K)(op: O with PrefixTreeLike[K,(U, Seq[(K,Repr)]) => U,O]): U = {
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
  def deepForeachFull(k:K)(op: ((K,Repr),=>Unit) => Unit): Unit = {
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
  def deepForeach[U](k:K)(op: ((K,Repr),Iterator[U]) => U): U = {
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
  def deepForeachRec1[U](k0:K)(op: (Seq[(K,Repr)],Iterator[U]) => U): U = {
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
  def deepForeachRec2[O<:PrefixTreeLike[K,(Seq[(K,Repr)],Iterator[Unit]) => Unit,O]](k0:K)(op: O with PrefixTreeLike[K,(Seq[(K,Repr)],Iterator[Unit]) => Unit,O]): Unit = {
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
    (this.asInstanceOf[TraversableOnce[(K,This)]]).map { case (k, v) => s"$k -> $v" }.addString(b, start, sep, end)

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

  abstract class Abstract[K,+V](val value:Option[V]) extends Traversable[(K,Abstract[K,V])] with PrefixTraversableOnce[K,V,Abstract[K,V]] {
    this:Abstract[K,V] =>
    override def stringPrefix: String = super[PrefixTraversableOnce].stringPrefix
  }

  def empty[K,V] = new Abstract[K,V](None) {
    def foreach[X](f0:((K,Abstract[K,V]))=>X):Unit = ()
  }

  //General note: the 16 following classes are increasingly complex versions of the last two classes.
  //The 14 first classes can be fully duplicated by the last two, albeit at a higher processing cost (expected,
  //not verified) and a very slightly higher memory footprint.
  //The last class defines the algorithm for generic recursion ; other classes just adapt it slightly.
  //For testing purposes, this means that the most important class to test is the last one.
  //The class that comes just before introduces the handling of defaults and direct translation to
  //PrefixTreeLike. That handling must also be thoroughly tested.
  //All other classes should of course be tested, but their is a high priority on the last two, in the
  //sense that there is a very strong reason to believe that once these two classes are bug-free, the other
  //will too...


  /** as Recur, simplified in cases where no external X data is involved and parents are not needed and the keys don't change
   */
  abstract class RecurNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R]) {
    type Data = (K,This)
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(Option[W],K=>R)
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This): K=>R = k => {
      recur((k,defa(k))) match {
        case null => bf.empty
        case r1   => r1._2
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._2) recur(z) match {
          case null =>
          case r1   => b += r1
        }
        (s._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = recur((k0,tree)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurView, simplified in cases where no external X data is involved and parent are not needed and the keys don't change
   */
  abstract class RecurViewNoDataSameKey[K, V, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = (K, This)
    type A = Abstract[K,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): Option[W]
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(K,A) = mapValues(s) match {
      case null => null
      case u    => (s._1,new A(u) {
        def foreach[X](g:((K,A))=>X):Unit = {
          for (z <- s._2) recur(z) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): A = recur((k0, tree)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRec simplified when no parents are needed and the keys don't change
   */
  abstract class RecurSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R]) {
    type Data = ((K,This),X)
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(Option[W],K=>R)
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child:(K,This),s:Data):X
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This): K=>R = k => {
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => bf.empty
        case x1   => recur((chld,x1)) match {
          case null => bf.empty
          case r1   => r1._2
        }
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)) match {
            case null =>
            case r1   => b += r1
          }
        }
        (s._1._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = recur(((k0,tree),x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRecView simplified when parents are not needed and the keys don't change
   */
  abstract class RecurViewSameKey[X, K, V, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = ((K, This), X)
    type A = Abstract[K,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): Option[W]
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child: (K, This), s: Data): X
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(K,A) = mapValues(s) match {
      case null => null
      case u    => (s._1._1,new A(u) {
        def foreach[X](g:((K,A))=>X):Unit = {
          for (z <- s._1._2) nextX(z, s) match {
            case null =>
            case x1 => recur((z, x1)) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): A = recur(((k0, tree), x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRec, simplified in cases where no external X data is involved and the keys don't change
   */
  abstract class RecurRecNoDataSameKey[K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R]) {
    type Data = Seq[(K,This)]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(Option[W],K=>R)
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This): K=>R = k => {
      recur((k,defa(k)) +: s) match {
        case null => bf.empty
        case r1   => r1._2
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._2) recur(z+:s) match {
          case null =>
          case r1   => b += r1
        }
        (s.head._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = recur(Seq((k0,tree))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRecView, simplified in cases where no external X data is involved and the keys don't change
   */
  abstract class RecurRecViewNoDataSameKey[K, V, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = Seq[(K, This)]
    type A = Abstract[K,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): Option[W]
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(K,A) = mapValues(s) match {
      case null => null
      case u    => (s.head._1,new A(u) {
        def foreach[X](g:((K,A))=>X):Unit = {
          for (z <- s.head._2) recur(z +: s) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): A = recur(Seq((k0, tree))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRec when the keys don't change
   */
  abstract class RecurRecSameKey[X,K,V,W,R<:PrefixTreeLike[K,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[K,W,R]) {
    type Data = Seq[((K,This),X)]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(Option[W],K=>R)
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child:(K,This),s:Data):X
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This): K=>R = k => {
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => bf.empty
        case x1   => recur((chld,x1)+:s) match {
          case null => bf.empty
          case r1   => r1._2
        }
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(K,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)+:s) match {
            case null =>
            case r1   => b += r1
          }
        }
        (s.head._1._1,b.result(u._1,u._2))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = recur(Seq(((k0,tree),x))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRecView when the keys don't change
   */
  abstract class RecurRecViewSameKey[X, K, V, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = Seq[((K, This), X)]
    type A = Abstract[K,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): Option[W]
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child: (K, This), s: Data): X
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(K,A) = mapValues(s) match {
      case null => null
      case u    => (s.head._1._1,new A(u) {
        def foreach[X](g:((K,A))=>X):Unit = {
          for (z <- s.head._1._2) nextX(z, s) match {
            case null =>
            case x1 => recur((z, x1) +: s) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): A = recur(Seq(((k0, tree), x))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as Recur, simplified in cases where no external X data is involved and parents are not needed
   */
  abstract class RecurNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R]) {
    type Data = (K,This)
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(L,Option[W],L=>R)
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = l => {
      val k = g(l)
      recur((k,defa(k))) match {
        case null => bf.empty
        case r1   => r1._2
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._2) recur(z) match {
          case null =>
          case r1   => b += r1
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = recur((k0,tree)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurView, simplified in cases where no external X data is involved and parent are not needed
   */
  abstract class RecurViewNoData[K, V, L, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = (K, This)
    type A = Abstract[L,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): (L, Option[W])
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(L,A) = mapValues(s) match {
      case null => null
      case u    => (u._1,new A(u._2) {
        def foreach[X](g:((L,A))=>X):Unit = {
          for (z <- s._2) recur(z) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): A = recur((k0, tree)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRec simplified when no parents are needed
   */
  abstract class Recur[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R]) {
    type Data = ((K,This),X)
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(L,Option[W],L=>R)
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child:(K,This),s:Data):X
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = l => {
      val k = g(l)
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => bf.empty
        case x1   => recur((chld,x1)) match {
          case null => bf.empty
          case r1   => r1._2
        }
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)) match {
            case null =>
            case r1   => b += r1
          }
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = recur(((k0,tree),x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** As RecurRecView simplified when parents are not needed
   */
  abstract class RecurView[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = ((K, This), X)
    type A = Abstract[L,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): (L, Option[W])
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child: (K, This), s: Data): X
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(L,A) = mapValues(s) match {
      case null => null
      case u    => (u._1,new A(u._2) {
        def foreach[X](g:((L,A))=>X):Unit = {
          for (z <- s._1._2) nextX(z, s) match {
            case null =>
            case x1 => recur((z, x1)) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): A = recur(((k0, tree), x)) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRec, simplified in cases where no external X data is involved
   */
  abstract class RecurRecNoData[K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R]) {
    type Data = Seq[(K,This)]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(L,Option[W],L=>R)
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = l => {
      val k = g(l)
      recur((k,defa(k))+:s) match {
        case null => bf.empty
        case r1   => r1._2
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._2) recur(z+:s) match {
          case null =>
          case r1   => b += r1
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,tree:This):R = recur(Seq((k0,tree))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** as RecurRecView, simplified in cases where no external X data is involved
   */
  abstract class RecurRecViewNoData[K, V, L, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = Seq[(K, This)]
    type A = Abstract[L,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): (L, Option[W])
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(L,A) = mapValues(s) match {
      case null => null
      case u    => (u._1,new A(u._2) {
        def foreach[X](g:((L,A))=>X):Unit = {
          for (z <- s.head._2) recur(z +: s) match {
            case null =>
            case r1   => g(r1)
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, tree: This): A = recur(Seq((k0, tree))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** The most generic class that highlights the recursion algorithm on PrefixTraversableOnce.
   *  Like RecurRecView, but the purpose is to build an actual PrefixTreeLike tree, not just
   *  a PrefixTraversableOnce (a view) ; it is as complex as its sibling, has most of the same
   *  properties, and adds the possibility to handle a default function.
   *  Not using RecurRecView also removes one layer of intermediate object (Abstract.)
   */
  abstract class RecurRec[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](implicit bf:PrefixTreeLikeBuilder[L,W,R]) {
    type Data = Seq[((K,This),X)]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur:Data):(L,Option[W],L=>R)
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child:(K,This),s:Data):X
    /** Building a recursive default from a K=>Repr default.
     *  This is not directly used by the recursion, but it provides a way
     *  to build the default value above from a K=>This default method.
     *  It comes handy when handling prefix trees.
     */
    def buildDefault(s:Data,defa:K=>This,g:L=>K): L=>R = l => {
      val k = g(l)
      val chld = (k,defa(k))
      nextX(chld,s) match {
        case null => bf.empty
        case x1   => recur((chld,x1)+:s) match {
          case null => bf.empty
          case r1   => r1._2
        }
      }
    }
    /**The recursive call to walk the tree
     * @param cur, the current node and node param values
     * @param defa, the default function for the node being built
     * @return a node value, which can be null (ignored)
     */
    def recur(s:Data):(L,R) = mapValues(s) match {
      case null => null
      case u    => val b=bf.newEmpty
        for (z <- s.head._1._2) nextX(z,s) match {
          case null =>
          case x1   => recur((z,x1)+:s) match {
            case null =>
            case r1   => b += r1
          }
        }
        (u._1,b.result(u._2,u._3))
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0:K,x:X,tree:This):R = recur(Seq(((k0,tree),x))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  /** The most generic class that highlights the recursion algorithm on PrefixTraversableOnce.
   *  Most methods will use it in derived form to achieve their own purpose.
   *  It has the following properties:
   *  - user methods mapValues and nextX can refer to the whole caller hierarchy,
   *    up to the top of the tree if necessary
   *  - user methods mapValues and nextX can both return null ; such a result indicates that
   *    the currently processed node will be ignored (and its children too) ; either acts as
   *    an embedded filter. If the top tree meets that filter condition, the final apply
   *    method (which builds the transformed tree) will throw NoSuchElementException.
   *  Because this recursion builds many intermediate objects (sequence of parents, pairs
   *  of ((K,This),X), and calls methods that are useless in many cases (e.g. when K=L or U=V)
   *  Other simpler and more efficient forms are provided.
   */
  abstract class RecurRecView[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]] {
    type Data = Seq[((K, This), X)]
    type A = Abstract[L,W]
    /** Building the value for the child
     *  @param cur, the child
     *  @return a node value, which can be null (child ignored)
     */
    def mapValues(cur: Data): (L, Option[W])
    /** Building the data for the child
     *  @param cur, the child
     *  @param x, the parent's data
     *  @return a data value, which can be null (child ignored)
     */
    def nextX(child: (K, This), s: Data): X
    /** The recursive call to walk the tree
     *  @param cur, the current node and node param values
     *  @param defa, the default function for the node being built
     *  @return a node value, which can be null (ignored)
     */
    def recur(s: Data):(L,A) = mapValues(s) match {
      case null => null
      case u    => (u._1,new A(u._2) {
        def foreach[X](g:((L,A))=>X):Unit = {
          for (z <- s.head._1._2) nextX(z, s) match {
            case null =>
            case x1   => recur((z, x1) +: s) match {
              case null =>
              case r1   => g(r1)
            }
          }
        }
      })
    }
    /** Running against the current tree.
     */
    @throws(classOf[NoSuchElementException])
    def apply(k0: K, x: X, tree: This): A = recur(Seq(((k0, tree), x))) match {
      case null => throw new NoSuchElementException
      case r    => r._2
    }
  }

  def apply[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]](v:Seq[((K, This), X)]=>(L, Option[W]), x:((K, This),Seq[((K, This), X)])=> X) = new RecurRecView[X,K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
    def nextX(child: (K, This), s: Data): X  = x(child,s)
  }
  def apply[K, V, L, W, This <: PrefixTraversableOnce[K, V, This]](v:Seq[(K, This)]=>(L, Option[W])) = new RecurRecViewNoData[K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
  }
  def apply[X, K, V, L, W, This <: PrefixTraversableOnce[K, V, This]](v:(((K, This), X))=>(L, Option[W]), x:((K, This),((K, This), X))=> X) = new RecurView[X,K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
    def nextX(child: (K, This), s: Data): X  = x(child,s)
  }
  def apply[K, V, L, W, This <: PrefixTraversableOnce[K, V, This]](v:((K, This))=>(L, Option[W])) = new RecurViewNoData[K,V,L,W,This] {
    def mapValues(s: Data): (L, Option[W])   = v(s)
  }
  def apply[X, K, V, W, This <: PrefixTraversableOnce[K, V, This]](v:Seq[((K, This), X)]=>Option[W], x:((K, This),Seq[((K, This), X)])=> X) = new RecurRecViewSameKey[X,K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
    def nextX(child: (K, This), s: Data): X  = x(child,s)
  }
  def apply[K, V, W, This <: PrefixTraversableOnce[K, V, This]](v:Seq[(K, This)]=>Option[W]) = new RecurRecViewNoDataSameKey[K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
  }
  def apply[X, K, V, W, This <: PrefixTraversableOnce[K, V, This]](v:(((K, This), X))=>Option[W], x:((K, This),((K, This), X))=> X) = new RecurViewSameKey[X,K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
    def nextX(child: (K, This), s: Data): X  = x(child,s)
  }
  def apply[K, V, W, This <: PrefixTraversableOnce[K, V, This]](v:((K, This))=>Option[W]) = new RecurViewNoDataSameKey[K,V,W,This] {
    def mapValues(s: Data): Option[W]   = v(s)
  }

  def apply[X,K,V,L,W,R<:PrefixTreeLike[L,W,R],This<:PrefixTraversableOnce[K,V,This]](v:Seq[((K, This), X)]=>(L, Option[W], L=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new RecurRec[X,K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
    def nextX(child: (K, This), s: Data): X      = x(child,s)
  }
  def apply[X, K, V, L, W, R<:PrefixTreeLike[L,W,R], This<:PrefixTreeLike[K,V,This]](v:Seq[((K, This), X)]=>(L, Option[W]), g : L=>K, x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new RecurRec[X,K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = { val z=v(s); (z._1,z._2,buildDefault(s,s.head._1._2.default,g)) }
    def nextX(child: (K, This), s: Data): X      = x(child,s)
  }
  def apply[K, V, L, W, R<:PrefixTreeLike[L,W,R], This <: PrefixTraversableOnce[K, V, This]](v:Seq[(K, This)]=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new RecurRecNoData[K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
  }
  def apply[K, V, L, W, R<:PrefixTreeLike[L,W,R], This <: PrefixTreeLike[K, V, This]](v:Seq[(K, This)]=>(L, Option[W]), g : L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new RecurRecNoData[K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = { val z=v(s); (z._1,z._2,buildDefault(s,s.head._2.default,g)) }
  }
  def apply[X, K, V, L, W, R<:PrefixTreeLike[L,W,R], This <: PrefixTraversableOnce[K, V, This]](v:(((K, This), X))=>(L, Option[W], L=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new Recur[X,K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
    def nextX(child: (K, This), s: Data): X      = x(child,s)
  }
  def apply[X, K, V, L, W, R<:PrefixTreeLike[L,W,R], This <: PrefixTreeLike[K, V, This]](v:(((K, This), X))=>(L, Option[W]), g: L=>K, x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new Recur[X,K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = { val z=v(s); (z._1,z._2,buildDefault(s,s._1._2.default,g)) }
    def nextX(child: (K, This), s: Data): X      = x(child,s)
  }
  def apply[K, V, L, W, R<:PrefixTreeLike[L,W,R], This <: PrefixTraversableOnce[K, V, This]](v:((K, This))=>(L, Option[W], L=>R))(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new RecurNoData[K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = v(s)
  }
  def apply[K, V, L, W, R<:PrefixTreeLike[L,W,R], This <: PrefixTreeLike[K, V, This]](v:((K, This))=>(L, Option[W]), g : L=>K)(implicit bf:PrefixTreeLikeBuilder[L,W,R]) = new RecurNoData[K,V,L,W,R,This] {
    def mapValues(s: Data): (L, Option[W], L=>R) = { val z=v(s); (z._1,z._2,buildDefault(s,s._2.default,g)) }
  }
  def apply[X, K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTraversableOnce[K, V, This]](v:Seq[((K, This), X)]=>(Option[W],K=>R), x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurRecSameKey[X,K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)    = v(s)
    def nextX(child: (K, This), s: Data): X      = x(child,s)
  }
  def apply1[X, K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTreeLike[K, V, This]](v:Seq[((K, This), X)]=>Option[W], x:((K, This),Seq[((K, This), X)])=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurRecSameKey[X,K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)    = (v(s),buildDefault(s,s.head._1._2.default))
    def nextX(child: (K, This), s: Data): X      = x(child,s)
  }
  def apply[K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTraversableOnce[K, V, This]](v:Seq[(K, This)]=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurRecNoDataSameKey[K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)    = v(s)
  }
  def apply1[K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTreeLike[K, V, This]](v:Seq[(K, This)]=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurRecNoDataSameKey[K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)    = (v(s),buildDefault(s,s.head._2.default))
  }
  def apply[X, K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTraversableOnce[K, V, This]](v:(((K, This), X))=>(Option[W], K=>R), x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurSameKey[X,K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)   = v(s)
    def nextX(child: (K, This), s: Data): X     = x(child,s)
  }
  def apply1[X, K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTreeLike[K, V, This]](v:(((K, This), X))=>Option[W], x:((K, This),((K, This), X))=> X)(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurSameKey[X,K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)   = (v(s), buildDefault(s,s._1._2.default))
    def nextX(child: (K, This), s: Data): X     = x(child,s)
  }
  def apply[K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTraversableOnce[K, V, This]](v:((K, This))=>(Option[W], K=>R))(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurNoDataSameKey[K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)   = v(s)
  }
  def apply1[K, V, W, R<:PrefixTreeLike[K,W,R], This <: PrefixTreeLike[K, V, This]](v:((K, This))=>Option[W])(implicit bf:PrefixTreeLikeBuilder[K,W,R]) = new RecurNoDataSameKey[K,V,W,R,This] {
    def mapValues(s: Data): (Option[W], K=>R)   = (v(s), buildDefault(s,s._2.default))
  }

}

