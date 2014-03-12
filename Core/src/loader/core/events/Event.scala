package loader.core.events

import loader.core.definition.Processor


/** Events are sent by the processor used, or by any callback associated with the run.
 *  They are often attached to a specific Element class.
 */
trait Event {
  //used for efficient dispatching in pattern matching through @switch.
  //overlaps can be managed by subclassing: see DefaultEvent and Exception events
  //idx must be >=0 when using array dispatchers.
  val idx:Int 
}


object Event {
  /*
   * We do not want gigantic switchs ; hence when possible we use contiguous idx to identify individual events.
   * Doing so loses the class info, which we have to restore by hand through dynamic casting.
   * This won't fail as long as the arrays are consistent with the idx definitions.
   * Thus, we just check a broad class of events (e.g. 'DefaultEvent' or 'Exception with Event'), then do an array dispatch.
   * This two steps dispatcher is reasonnably fast and scales well.
   */
  abstract class Dispatcher[-P<:Processor,+Evt<:Event,+X] {
    def apply(x:(P#Elt,Event)):X = process(x.asInstanceOf[(P#Elt,Evt)])  //XXX justify cast
    protected[this] def process(e:(P#Elt,Evt)):X
  }

  /** Defines an event dispatcher partial function by matching both a super-class (Evt) and an array of Builder.
   *  This is useful for a reasonably fast dispatching of events.
   */
  class DispatchByClassArray[-P<:Processor,-Evt<:Event:Manifest,+X](a:Array[_<:Dispatcher[P,Evt,X]]) extends PartialFunction[(P#Elt,Event),X] {
    protected[this] val m = manifest[Evt]
    def isDefinedAt(x:(P#Elt,Event)):Boolean = m.runtimeClass.isAssignableFrom(x._2.getClass) && x._2.idx<a.length && x._2.idx>=0
    def apply(x:(P#Elt,Event)):X = a(x._2.idx)(x)
  }

}
