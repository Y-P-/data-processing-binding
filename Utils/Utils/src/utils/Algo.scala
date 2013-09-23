package utils

object Algo {
  
  /**
   * Merges two lists which are in ascending order and builds a new list
   * which is in ascending order containing all the elements from both lists.
   * @param first list
   * @param second list
   * @param order operation lt(x1,x2)==true <=> x1<x2
   * @return the merged ordered list
   */
  def merge[X<:AnyRef](left:Iterable[X],right:Iterable[X],lt:(X,X)=>Boolean):List[X] = {
    import scala.collection.mutable.Buffer
    //assumes _1, _2 are in ascending order (_1.head < _1.last  <=> cmp(head,last)<0)
    //assumes _3 is inferior to both _1 and _2 and descending order
    //the result of merge1 is a step that removes an element from either _1 or _2 and puts it in _3
    //the result conserves the initial properties
    def merge1(v:(Iterable[X],Iterable[X],Buffer[X])):(Iterable[X],Iterable[X],Buffer[X]) = {
      if (v._1.isEmpty)                    (Nil,       Nil,       v._3 ++= v._2)
      else if (v._2.isEmpty)               (Nil,       Nil,       v._3 ++= v._1)
      else if (lt(v._1.head,v._2.head))    (v._1.tail, v._2,      v._3 += v._1.head)
      else                                 (v._1,      v._2.tail, v._3 += v._2.head)
    }
    var r = (left,right,Buffer[X]())
    while (!r._1.isEmpty || !r._2.isEmpty) r = merge1(r)
    r._3.toList
  }
  /**
   * Returns the last element in an "ordered" list which satisfies a predicate.
   * This is effecient because this works by dichotomy.
   * @param l ordered list in which all the first elements do satisfy the predicate, and the last ones don't.
   * @param f predicate
   * @return the last element which satisfies the predicate
   */
  def dicho[X](list:Iterable[X],f:(X)=>Boolean):Option[X] = {
    import scala.annotation.tailrec
    @tailrec def simple(l:Iterable[X]):Option[X] =
      if (l.size==1) Some(l.head) else simple(
        l.splitAt(l.size/2) match {
          case (x,y) if f(y.head) => y
          case (x,y)              => x
        })
    if      (list.isEmpty)  None
    else if (!f(list.head)) None
    else                    simple(list)
  }
}