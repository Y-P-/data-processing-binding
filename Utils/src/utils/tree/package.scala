package utils

import scala.annotation.switch

package object tree {
  sealed class Strictness(val op_strict:Boolean, val tree_strict:Boolean) {
    @inline final def succeeds[K](t:PrefixTreeLike[K,_,_],o:PrefixTreeLike[K,_,_])(k:K) =
      (!tree_strict || t.isDefinedAt(k)) && (!op_strict || o.isDefinedAt(k))
    final def apply[K,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,_,O]](k:K,t:T,o:O) = {
      try { if (succeeds(t,o)(k)) { val r=(t(k),o(k)); if (r._1==null||r._2==null) null else r } else null } catch { case _:NoSuchElementException=> null }
    }
  }
  val OP_STRICT    = new Strictness(true,false)
  val RIGHT_STRICT = new Strictness(false,true)
  val FULL_STRICT  = new Strictness(true,true)
  val NOT_STRICT   = new Strictness(false,false)

  sealed class MergeMode(id:Int) {
    def apply[K,V,T<:PrefixTreeLike[K,V,T]](r1:T,r2:T,mergeValues:(Option[V],Option[V])=>Option[V])(implicit bf:PrefixTreeLikeBuilder[K,V,T]):T = {
      (id: @switch) match {
          case 0 => r1
          case 1 => r2
          case 2 => r1.merge(r2,false,mergeValues,false)
          case 3 => r1.merge(r2,true,mergeValues,false)
          case 4 => r2.merge(r1,false,mergeValues,false)
          case 5 => r2.merge(r1,true,mergeValues,false)
        }
    }
  }
  /** keep first node */
  val KEEP                    = new MergeMode(0)
  /** keep second node */
  val REPLACE                 = new MergeMode(1)
  /** merge values and common children (2 on 1), keep different children */
  val MERGE                   = new MergeMode(2)
  /** merge values and keep different children. second node children replace first node ones when common */
  val MERGE_OVERWRITE         = new MergeMode(3)
  /** merge values and common children (1 on 2), keep different children */
  val MERGE_REVERSE           = new MergeMode(4)
  /** merge values and keep different children. first node children stay when common */
  val MERGE_REVERSE_OVERWRITE = new MergeMode(5)

}