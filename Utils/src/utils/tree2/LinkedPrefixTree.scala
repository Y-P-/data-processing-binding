package utils.tree2

/** Working with Prefix Trees based on a LinkedHashMap
 */
object LinkedPrefixTree extends PrefixTreeLikeBuilder.GenBuilder2[PrefixTree] {
  implicit def builder[K,V] = PrefixTree(scala.collection.mutable.LinkedHashMap.empty[K,PrefixTree[K,V]])(true)
}
