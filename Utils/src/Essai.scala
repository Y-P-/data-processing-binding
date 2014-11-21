object Essai {

  trait P[+V, This <: P[V, This]] { this:This=>
    def next:This = null.asInstanceOf[This]
    def foo[U](op:P[This=>U,_]):P[This=>U, _] = {
      val o = op.next  //Any   => should be _ <: P[This=>U, _]
//      o.next           //Fails
      o.asInstanceOf[P[This=>U, _]].next.asInstanceOf[P[This=>U, _]]  //!!!!!!
    }
    def bar[U,O<:P[This=>U,O]](op:O):O = {
      val o = op.next
      o.next
    }
  }
  
  class P0[V] extends P[V,P0[V]]
  
  val p0 = new P0[Int]
  val p1 = new P0[P0[Int]=>String]
  p0.foo(p1)
//  p0.bar(p1)                               //Fails
  p0.bar[String,P0[P0[Int]=>String]](p1)   //Right
  
}