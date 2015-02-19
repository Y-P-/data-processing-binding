package utils.tree

/** Throws in the ability to mutate a PrefixTreeLike.
 */
trait MutablePrefixTreeLike[K,V,T<:MutablePrefixTreeLike[K,V,T]] extends PrefixTreeLike[K,V,T] { this:T=>
  def value_=(v:Option[V]):Unit
  def default_=(d:K=>Repr):Unit
  def update(k:K,t:T):Unit
}
