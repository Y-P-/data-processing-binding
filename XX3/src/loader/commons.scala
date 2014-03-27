package loader

/** Groups tiny classes that are widely used and don't find a right place anywhere.
 *  The definitions are usually lifted to the package object.
 */
object commons {
  
  
  /**
   * Indicates that the Loadable cares for the actual name of the tag.
   * Mostly useful for seqs with regex tags.
   */
  trait Named {
    def name_=(name:String):Unit 
  }  
}