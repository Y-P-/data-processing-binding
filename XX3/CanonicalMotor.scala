package loader.motors.canonical

import loader._
import scala.collection.mutable.ListBuffer

class Motor[A,B]
class StructField
class RootEngine[A]
abstract class Loader {
  def name:String
  def isInclude:Boolean
}


abstract class CanonicalMotor[-E<:ElementBase#Elt] extends Motor[Unit,Unit] {
  def action:ElementBase.Action[E]
  def executor:ElementBase.Executor[E]
}

abstract class ElementBase {
  type Element>:Null<:Elt
  //a trait to recursively encapsulate objects
  trait Layered {
    def name:String
    def parent:Element
    def execute()
  }
  trait Valued extends Layered {
    def value:String
  }
  trait Elt extends Layered {self:Element=>
    type Attribute<:Attr
    type Terminal<:Term
    def name:String
    protected[this] def motor:CanonicalMotor[Element]
    private[this] lazy val act = motor.action
    protected[this] val exc:ElementBase.Executor[Element]
    abstract protected trait Inner extends Layered {
      final override def parent = Elt.this
    }
    /** Intermediate container classes.
     *  These fit simple purpose ; you may derive them, especially if you have to keep additional inforamtion.
     */
    trait Term  extends Valued {this:Terminal=>
      def name:String
      def value:String
      def execute() = act.onTerminal(this)
      override def toString = parent.toString+" >> "+name+" = "+value
    }
    trait Attr extends Valued {this:Attribute=>
      def name:String
      def value:String
      def execute() = act.onAttribute(this)
      override def toString = parent.toString+" >> "+name+" = "+value
    }
    trait InnerElt extends Elt with Inner {this:Element=>
      protected[this] def motor = self.motor
      protected[this] val exc:ElementBase.Executor[Element] = self.exc(this)  //build a new execution context 
      override def toString = self.toString+" >> "+name
    }
    
    /** Factories ; you may override these, especially when you require other info from the Loader.
     *  Be aware however, that if you're not carefull,you might end up dragging the whole Loader tree if
     *  you keep any reference to Loader. This may be memory costly.
     */
    def terminal(ld:Loader,value0:String):Terminal    //building a terminal
    def attribute(ld:Loader,value0:String):Attribute  //building an attribute
    def element(ld:Loader):Element                    //building a non terminal element
    /** Receiving fields.
     */
    final def terminal0(ld:Loader,value:String):Terminal   = { val t=terminal(ld,value);  exc.recvField(t); t} //receiving a terminal
    final def attribute0(ld:Loader,value:String):Attribute = { val a=attribute(ld,value); exc.recvAttr(a);  a} //receiving an attribute
    final def element0(ld:Loader):Element                  = {                                                 //receiving a non terminal field
      if (ld.isInclude) {  //beware includes! we must recover the original element from the motor before the include ; otherwise we get two times the top item
        //ld.parent.stk.current.motor.asInstanceOf[BaseCanonicalMotorEngine].elt
        null
      } else {
        val e=element(ld)
        exc.recvElt(e)
        e
      }
    }
    def begin():Unit  = act.begin(this)    //code executed before processing the attributes
    def middle():Unit = act.middle(this)   //code executes before processing the fields
    def close():Unit  = act.close(this)    //code executed on end
    def exit():Unit   = exc.exitElt(this)  //    
  }
}

object ElementBase {
  /** Defines what you actually do with objects.
   *  The framework (Executor) warrants that:
   *  - any call to onAttribute will be preceeded by the container's begin
   *  - any call to middle will be preceeded by begin
   *  - any call to onElement will be preceeded by the container's middle (hence begin too)
   *  - any call to close will be preceeded by middle (hence begin)
   *  - close will be called
   *  - calls to onAttribute will be ignored after any call to middle
   *  In effect, you only have to fill in what you want each method to do: they will be invoked
   *  in the correct order.
   */
  trait Action[-E<:ElementBase#Elt] {
    def onAttribute (a:E#Attribute):Unit  //what you do with an Attribute
    def onTerminal  (t:E#Terminal):Unit  //what you do with a Terminal
    def begin       (elt:E):Unit
    def close       (elt:E):Unit
    def middle      (elt:E):Unit
  }
  trait Executor[-E<:ElementBase#Elt] {
    def recvAttr(e:E#Attribute)
    def recvField(e:E#Terminal)
    def recvElt(e:E)
    def exitElt(e:E)
    def onElement(e:E)
    def apply(e:E):Executor[E]
  }
  class Standard extends ElementBase {
    abstract class Element(val name:String) extends Elt {
      protected[this] def motor:CanonicalMotor[Element]
      abstract class Attribute(val name:String, val value:String) extends Attr
      abstract class Terminal(val name:String, val value:String) extends Term
      def execute() = exc.onElement(this)
      def attribute(ld:Loader,value:String) = new Attribute(ld.name,value) with Inner
      def terminal(ld:Loader,value:String)  = new Terminal(ld.name,value) with Inner
      def element(ld:Loader)                = new Element(ld.name) with InnerElt
    }
  }
  
  object Executor {
    //loads everything before anything is output
    //attributes are correctly handled if out of order
    //consumes lots of memory
    class Delayed extends Executor[ElementBase#Elt] {
      import scala.collection.mutable.ListBuffer
      def onElement(elt:ElementBase#Elt) {
        elt.begin
        for (a <- attrs) a.execute
        elt.middle
        for (e <- elts) e.execute
        elt.close
      }
      private val attrs = ListBuffer[ElementBase#Elt#Attr]()
      private val elts  = ListBuffer[ElementBase#Layered]()
      def recvAttr(e:ElementBase#Elt#Attribute) = attrs += e
      def recvField(e:ElementBase#Elt#Terminal) = elts  += e
      def recvElt(e:ElementBase#Elt)            = elts  += e 
      def exitElt(e:ElementBase#Elt) = if (e.parentStc==null) onElement(e)
      def apply(e:ElementBase#Elt)   = new Delayed
    }
    //writes elements as they are received
    //attributes that come in an untimely fashion are dismissed
    class Seq(val elt:ElementBase#Elt,val parent:Seq) extends Executor[ElementBase#Elt] {
      def this(elt:ElementBase#Elt) = this(elt,null)
      private var etat:Int = 0  //0:nothing done,1:begin done,2:middle done,3:end done
      def state(expected:Int):Unit = if (etat<expected) expected match {
        case 1 => if (parent!=null) parent.state(2); elt.begin;  etat=1
        case 2 => state(1); elt.middle; etat=2
        case 3 => state(2); elt.close;  etat=3
      }
      def recvAttr(e:ElementBase#Elt#Attribute) = { state(1); e.execute }
      def recvField(e:ElementBase#Elt#Terminal) = { state(2); e.execute }
      def recvElt(e:ElementBase#Elt)            = state(1)
      def exitElt(e:ElementBase#Elt)            = state(3)
      def apply(e:ElementBase#Elt)              = new Seq(e,this)
      def onElement(e:ElementBase#Elt)          = ()
      override def toString = "SeqExecutor for "+elt
    }
  }
}