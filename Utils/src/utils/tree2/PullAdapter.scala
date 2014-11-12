package utils.tree2
import com.typesafe.config.ConfigFactory
import akka.actor.{Actor,Terminated,ActorSystem,Props,ReceiveTimeout}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.Future


class PullAdapter[K,V](f:(PullAdapter[K,V]#Layer, ()=>Unit)=>Unit) extends PushPull[K,V] {
  
  val waitTimeout = scala.concurrent.duration.Duration("1000ms")
  implicit val timeout = Timeout(1000,java.util.concurrent.TimeUnit.MILLISECONDS)
  
  case class Value(v:V)
  case class Key(k:K)
  
  //Config used for the Parser/Motor Actor pair. It ensures best performances (basic 1200msg/ms on i5-2400@3.1GHz).
  val config = """
    dispatcher {
      type=Dispatcher
      fork-join-executor {
        parallelism-max = 1
      }
    }
    """
  val sys = ActorSystem("PushPull", ConfigFactory.parseString(config))
  val p0 = Props(classOf[Top],this).withDispatcher("dispatcher")
  val p1 = Props(classOf[Layer],this).withDispatcher("dispatcher")


  class Layer extends Actor {
    import context._
    @volatile var key:K = _
    @volatile var value:V = _    
     
    def receive() = {
      case PullAdapter.Child => cur   = context.actorOf(p1); println("child")
      case Key(k)            => key   = k; println(key)
      case Value(v)          => value = v; println(value)
      case PullAdapter.End   => println("return"); cur=context.parent; context.parent ! this
      case l:Layer           => println("layer received")
    }
  }
  
  class Top extends Layer {
    override def receive() = {
      case PullAdapter.End  => println("finished"); sys.shutdown()
    }
  }
  
  private val top = sys.actorOf(p0)
  @volatile protected var cur = top
  
  final def push(k:K) = { cur ! PullAdapter.Child; cur ! Key(k) }
  final def pull(v:V) = cur ! Value(v)
  final def pull      = cur ! PullAdapter.End
  def run(x: =>Unit) = {
      x
  }
}

object PullAdapter {
  object Child
  object End
  
  def main(args:Array[String]):Unit = {
    val s = new PullAdapter[String,String]((x,f) => println(x.key+"/"+x.value+"{"+f()+"}"))
    s.run{
      s.push("a")
      s.pull
      s.push("aa")
      s.pull("1")
      s.pull
      s.pull
    }
  }
}