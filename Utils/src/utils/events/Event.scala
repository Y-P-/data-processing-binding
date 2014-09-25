package utils.events

import scala.collection.mutable.ArrayBuffer
import java.io.Closeable

trait Tracker {
  protected[this] type Tracking <: Tracked
  private[this] var ref:ArrayBuffer[Tracking] = null
  def +=(k:Tracking) = { if (ref==null) ref=ArrayBuffer[Tracking](); ref += k }
  def -=(k:Tracking) = { ref-=k; if (ref.isEmpty) ref=null }
  def isTracked = ref!=null
  def foreach[U](f:Tracking=>U) = if (ref!=null) ref.foreach(f)  
}
trait Tracked extends Closeable { self=>
  def tracker:Tracker { type Tracking>:self.type }
  tracker += this
  def close() = tracker -= this
}

trait Tracker2 {
  protected[this] type Tracking2 <: Tracked2
  private[this] var ref:ArrayBuffer[Tracking2] = null
  def +=(k:Tracking2) = { if (ref==null) ref=ArrayBuffer[Tracking2](); ref += k }
  def -=(k:Tracking2) = { ref-=k; if (ref.isEmpty) ref=null }
  def isTracked = !ref.isEmpty
  def foreach[U](f:Tracking2=>U) = if (ref!=null) ref.foreach(f)
}
trait Tracked2 extends Tracked { self=>
  def tracker2:Tracker2 { type Tracking2>:self.type }
  tracker2 += this
  override def close() = { tracker2 -= this; super.close() }
}


//an event ; usually an object
class Event extends Tracker {
  protected[this] type Tracking = Emitter[_,_]
  //an emitter for observing the event itself
  val self = new Emitter[Any,Null](this) {
    def source = null
  }  
}

//an emitter attached to that event and a given source
abstract class Emitter[-A,+S<:EventSource[_]](val event:Event) extends Tracked with Tracker {
  protected[this] type Tracking = Watcher[A,S,_]
  
  def source:S
  def tracker = event
  def emit(a:A):Unit = {
    event.self.foreach(_.receive(a))
    foreach(_.receive(a))
  }
}

//a watcher for that specific emitter
class Watcher[-A,+S<:EventSource[_],+R](val emitter:Emitter[A,S])(f:(Emitter[A,S],A)=>R) extends Tracked  {
  def tracker = emitter
  def receive(a:A):R = f(emitter,a)
}

//a source of events ; contains event emitters
class EventSource[+S](val source:S) extends Tracker2 {
  protected[this] type Tracking2 = Emitter[_,this.type] with Tracked2
  
  //event associated with firing this source
  val self = new Event
  
  //attaches an event to that source
  def attach[A](e:Event):Emitter[A,this.type] = new Emitter[A,this.type](e) with Tracked2 {
    def tracker2 = EventSource.this
    def source:EventSource.this.type = EventSource.this
    override def emit(a:A):Unit = { super.emit(a); self.self.emit(null) }
  }
}

//an observer of events
class Observer extends Tracker2 {
  protected[this] type Tracking2 = W[_,_,_]
  
  protected[this] class W[R,A,S<:EventSource[_]](emitter:Emitter[A,S])(f:(Emitter[A,S],A)=>R) extends Watcher[A,S,R](emitter)(f) with Tracked2 {    
    def tracker2 = Observer.this
	}

  //builds an observer for an emitter ; this is a precise observation
	def obs[R,A,S<:EventSource[_]](em:Emitter[A,S])(f:(Emitter[A,S],A)=>R):Watcher[A,S,R] = new W(em)(f)
  //builds an observer for an event
	def obs[R](ev:Event)(f:(Emitter[_,_],Any)=>R):Watcher[Any,Null,R] = new W(ev.self)(f)
}

