package loader.core.callbacks

  
import scala.collection.{Map,MapProxy}

/** A tree of callbacks, to find when to apply one.
 *  Note the global contra-variance on all parameters.
 */
class Callbacks[-E0,-S0,-R0,K,V>:Null](val name: String, val cur: Option[Callback[E0,S0,R0,K,V]], val self: Map[String, Callbacks[E0,S0,R0,K,V]]) extends utils.TreeLike[String, Callback[E0,S0,R0,K,V], Callbacks[E0,S0,R0,K,V]] with MapProxy[String, Callbacks[E0,S0,R0,K,V]] {
  def parent = throw new IllegalStateException //we don't have to climb back the tree
  protected[this] val builder = new CallbacksBuilder[E0,S0,R0,K,V]
}



