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
    def apply[K,V,T<:PrefixTreeLike[K,V,T]](r1:T,r2:T,useDefault:Boolean)(implicit bf:PrefixTreeLikeBuilder[K,V,T]):T = {
      (id: @switch) match {
          case 0 => r1
          case 1 => r2
          case 2 => r1.merge(r2,false,useDefault)
          case 3 => r1.merge(r2,true,useDefault)
          case 4 => r2.merge(r1,false,useDefault)
          case 5 => r2.merge(r1,true,useDefault)
        }
    }
  }
  val KEEP                    = new MergeMode(0)
  val REPLACE                 = new MergeMode(1)
  val MERGE                   = new MergeMode(2)
  val MERGE_OVERWRITE         = new MergeMode(3)
  val MERGE_REVERSE           = new MergeMode(4)
  val MERGE_REVERSE_OVERWRITE = new MergeMode(5)

}