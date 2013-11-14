package loader

/** Groups tiny classes that are widely used and don't find a right place anywhere.
 *  The definitions are usually lifted to the package object.
 */
object commons {
  
  trait Assoc[+K,+T] {
    def key:K
    def value:T
    final override def toString = "Assoc("+key+" -> "+value+")"
  }
  /** Used to map a pair key/value to store maps.
   *  Use this as the return type of an end method for items stored as maps.
   */
  final case class AssocElt[+K,+T](final val key:K, final val value:T) extends Assoc[K,T] {
    final protected def this() = this(null.asInstanceOf[K],null.asInstanceOf[T])
  }
  /** Use --> instead of -> for building Assoc as a shortcut
   */
  final implicit class AssocLeft[K](final val key:K) {
    @inline def -->[T] (t:T) = new AssocElt(key,t)
  }
  
  /**
   * Indicates that the Loadable cares for the actual name of the tag.
   * Mostly useful for seqs with regex tags.
   */
  trait Named {
    def name_=(name:String):Unit 
  }  
}