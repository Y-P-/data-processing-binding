package loader

/** Groups tiny classes that are widely used and don't find a right place anywhere.
 *  The definitions are usually lifted to the package object.
 */
object commons {
  
  /**
   * Used to map a pair key/value to store maps.
   * Use this as the return type of an end method for items stored as maps.
   */
  final case class Assoc[+K,+T](final val key:K, final val value:T) {
    final protected def this() = this(null.asInstanceOf[K],null.asInstanceOf[T])
    final override def toString = "Assoc("+key+" -> "+value+")"
  }
  
  /**
   * Indicates that the Loadable cares for the actual name of the tag.
   * Mostly useful for seqs with regex tags.
   */
  trait Named {
    def name_=(name:String):Unit 
  }

  /**
   * A class that must be used for Named Fields
   */
  final class NamedField[X](val value:X, var name:String) extends Named {
    protected def this() = this(null.asInstanceOf[X],null)
    override def toString = s"($name,$value)"
  }
  
  /**
   * An interface for managing sequences/lists.
   * Note: Some motors don't require keeping useless clutter (XML printing for exemple).
   *       This is why this feature is delegated.
   */
  abstract class Seq {
    def recover:AnyRef
    def size:Int
    def push(value:Any):Unit
    def fd:Context#FieldMapping
    def prepare() = current0=size+1
    final def current = current0
    private final var current0:Int=0
  }
  
}