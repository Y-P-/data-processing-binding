package utils

package object tree {
  sealed class Strictness(val op_strict:Boolean, val tree_strict:Boolean) {
    @inline final def succeeds[K](t:PrefixTreeLike[K,_,_],o:PrefixTreeLike[K,_,_])(k:K) =
      (!tree_strict || t.isDefinedAt(k)) && (!op_strict || o.isDefinedAt(k))
    final def apply[K,T<:PrefixTreeLike[K,_,T],O<:PrefixTreeLike[K,_,O]](k:K,t:T,o:O) = {
      try { if (succeeds(t,o)(k)) (t(k),o(k)) else null } catch { case _:NoSuchElementException=> null }
    }
  }
  val OP_STRICT    = new Strictness(true,false)
  val RIGHT_STRICT = new Strictness(false,true)
  val FULL_STRICT  = new Strictness(true,true)
  val NOT_STRICT   = new Strictness(false,false)

  sealed class MergeMode(val id:Int)
  val KEEP                    = new MergeMode(0)
  val REPLACE                 = new MergeMode(1)
  val MERGE                   = new MergeMode(2)
  val MERGE_OVERWRITE         = new MergeMode(3)
  val MERGE_REVERSE           = new MergeMode(4)
  val MERGE_REVERSE_OVERWRITE = new MergeMode(5)
}