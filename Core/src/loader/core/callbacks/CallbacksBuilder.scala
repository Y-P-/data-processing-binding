package loader.core.callbacks

/** The factory for callbacks. Think: fromCanonical
 */
class CallbacksBuilder[E0,S0,R0,K0,V>:Null] extends utils.TreeLikeBuilder[String, Callback[E0,S0,R0,K0,V], Callbacks[E0,S0,R0,K0,V]] {
  import scala.collection.mutable.Map
  type Repr[+t] = utils.TreeLike[String, t, _]
  type This = Callbacks[E0,S0,R0,K0,V]
  def mapBuild[S](name: String, cur: Option[S], next: Map[String, Repr[S]], prev: => Repr[S]): Repr[S] = throw new IllegalStateException
  def build(name: String, cur: Option[Callback[E0,S0,R0,K0,V]], next: Map[String, This], prev: => This): This = new Callbacks(name, cur, next)
  override def emptyMap[t] = utils.RegexMap(scala.collection.mutable.LinkedHashMap.empty[String, t]) //using default regex detector
  //builds a recursive callback
  def apply(c: Callback[E0,S0,R0,K0,V]): Callbacks[E0,S0,R0,K0,V] = {
    var cbk: Callbacks[E0,S0,R0,K0,V] = null
    cbk = new Callbacks(null, Some(c), cbk) { override def get(name: String) = Some(this) }
    cbk
  }
}