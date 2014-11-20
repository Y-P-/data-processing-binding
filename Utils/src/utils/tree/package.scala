package utils

package object tree {
  sealed class Strictness(val op_strict:Boolean, val tree_strict:Boolean) {
    @inline final def succeeds[K](t:PrefixTreeLike[K,_,_],o:PrefixTreeLike[K,_,_])(k:K) =
      (!tree_strict || t.isDefinedAt(k)) && (!op_strict || o.isDefinedAt(k))
  }
  val OP_STRICT    = new Strictness(true,false)
  val RIGHT_STRICT = new Strictness(false,true)
  val FULL_STRICT  = new Strictness(true,true)
  val NOT_STRICT   = new Strictness(false,false)

}