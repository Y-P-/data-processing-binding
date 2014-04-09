package loader.test.core

import utils.LogTester._
import utils.stringContextes._
import utils.RegexReplacer
import loader._
import loader.core.run
import loader.context.ClassContext
import loader.core.CtxCore
import loader.core.definition.Status
import loader.core.callbacks.CallbacksBuilder
import loader.core.ParserBuilder
import loader.features.{DefaultAuditHandler,DefaultCtxEventsCbk,StandardAuditLogger}
import loader.audit.{AuditRecorder,IdScheme}
import java.io.OutputStream
import java.io.PrintWriter
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import loader.test.Data
import loader.core.events.EventHandler
import loader.core.definition.Processor
import loader.annotations._
import loader.motors.ObjectMotor
import loader.core.ExtCore
import utils.StringConverter
import utils.reflect.ConversionSolver
import utils.reflect.Converters._
import utils.reflect.Binder
import utils.reflect.DataActor._
import utils.reflect.CollectionAdapter

/** The purpose of this test is to check that each layer of EltCtx is correctly
 *  found, and that there are no erroneous access to the parent layer.
 *  This test is particularly important for the ObjectMotor processor, which relies heavily on such data.
 *  This is not tested elsewhere as we usually rely on constant values across layers.
 *  Note here how each layer is severely constrained both on the availability of converters and on
 *  what it can spawn, get (as in converters(key)) or convert. 
 */
object ObjDepthTest {
  
  class L0 {
    var l1:L1 = _
    override def toString = s"l1= { $l1 } "
  }
  class L1 {
    var l2:L2 = _
    var l0:Array[L0] = _
    override def toString = s"l2= { $l2 } ${if (l0!=null) l0.mkString("["," ","]") else ""}"
  }
  class L2 {
    var id:Int = _
    var ids:Array[Int] = _
    override def toString = s"id=$id ids={ ${ids.mkString(" ")} } "
  }
  
  def load(rsc:String) = getClass.getResource(rsc) match {
    case null => throw new java.io.IOException(s"resource $rsc could not be found")
    case url  => url
  }
  
  val cvInt   = utils.ClassMap[FromString[_]](CvvInt)
  val cvCol   = new ConversionSolver(Map.empty.get,null,null,CollectionAdapter())
  val cvL2id  = new ConversionSolver(cvInt,null,null,Map.empty.get)
  val cvL2ids = new ConversionSolver(cvInt,null,null,CollectionAdapter())
  val noCv    = new ConversionSolver(Map.empty.get,null,null,Map.empty.get)
  
  // Prepare extremely specific contexts
  val L0Ctx = new ObjectMotor.EltCtx {
    override def toString                           = "L0Ctx"
    override def converters                         = cvCol   //for collection in L1.l0
    override def spawn(i:Binder#I)                  = ???     //should not be called: case (1) is root, L0 is provided. case two is collection: we don't use spawn.
    override def converters(key:String)             = noCv
    override def dataActor(cz:Class[_],name:String) = new FldElt(classOf[L0].getDeclaredField("l1"))
  }
  val L0l1Ctx = new ObjectMotor.EltCtx {
    override def toString                           = "L0l1Ctx"
    override def converters                         = noCv
    override def spawn(i:Binder#I)                  = super.spawn(i) // we could write new L1, but this uses inference
    override def converters(key:String)             = key match {
      case "l2" => L1l2Ctx.converters
      case "l0" => L0Ctx.converters
    }
    override def dataActor(cz:Class[_],name:String) = new FldElt(classOf[L1].getDeclaredField(name))
  }
  val L0l1l0Ctx = new ObjectMotor.EltCtx {
    override def toString                           = "L0l1l0Ctx"
    override def converters                         = noCv
    override def spawn(i:Binder#I)                  = super.spawn(i) //new L0
    override def converters(key:String)             = noCv
    override def dataActor(cz:Class[_],name:String) = super.dataActor(cz,name)
  }
  val L1l2Ctx = new ObjectMotor.EltCtx {
    override def toString                           = "L1l2Ctx"
    override def converters                         = noCv
    override def spawn(i:Binder#I)                  = super.spawn(i) //new L2
    override def converters(key:String)             = key match {
      case "id"  => L2idCtx.converters
      case "ids" => L2idsCtx.converters
    }
    override def dataActor(cz:Class[_],name:String) = new FldElt(classOf[L2].getDeclaredField(name))
  }
  val L2idCtx = new ObjectMotor.EltCtx {
    override def toString                           = "L2idCtx"
    override def converters                         = cvL2id
    override def spawn(i:Binder#I)                  = ???
    override def converters(key:String)             = ???
    override def dataActor(cz:Class[_],name:String) = ???
  }
  val L2idsCtx = new ObjectMotor.EltCtx {
    override def toString                           = "L2idsCtx"
    override def converters                         = cvL2ids
    override def spawn(i:Binder#I)                  = ???
    override def converters(key:String)             = ???
    override def dataActor(cz:Class[_],name:String) = ???
  }
  
  //Load contexts into a tree
  //as we do not yet have trees with loop, we must repeat for recursivity: stop at one level deep.
  //XXX check for missing elements ???
  import utils.stringContextes._
  val r = utils.Tree(List((split"/",                      L0Ctx),
                          (split"/l1",                    L0l1Ctx),
                          (split"/l1/l2",                 L1l2Ctx),
                          (split"/l1/l0",                 L0Ctx),
                          (split"/l1/l0/",                L0l1l0Ctx),
                          (split"/l1/l0//l1",             L0l1Ctx),
                          (split"/l1/l0//l1/l2",          L1l2Ctx),
                          (split"/l1/l0//l1/l2/id",       L2idCtx),
                          (split"/l1/l0//l1/l2/ids",      L2idsCtx),
                          (split"/l1/l0//l1/l2/ids/",     L2idCtx),
                          (split"/l1/l2/id",              L2idCtx),
                          (split"/l1/l2/ids",             L2idsCtx),
                          (split"/l1/l2/ids/",            L2idCtx)))
  
  //prepare default user-context: it will use the tree for specific elements
  abstract class Defaults[P<:ParserBuilder {type Value=String; type Key=String}, M<:ObjectMotor.DefImpl]
                 (tree: utils.Tree[String,ObjectMotor.EltCtx],default:ObjectMotor.EltCtx)
                 extends loader.core.UsrCtx[P,M] with ObjectMotor.UCtx[P,M] {
    type EltCtx >: Null <: EltCtxBase
    final def apply(elt:Proc#Elt):EltCtx = elt.parent match {
      case null => this(elt,tree(""))
      case p    => this(elt,p.eltCtx.asInstanceOf[EltCtx].tree(elt.name))
    }
  
    protected def apply(elt:Proc#Elt,tree:utils.Tree[String,ObjectMotor.EltCtx]):EltCtx
    
    abstract class EltCtxBase(val elt:Proc#Elt,val tree:utils.Tree[String,ObjectMotor.EltCtx]) extends super.EltCtxBase { this:EltCtx=>
      val eCtx = tree.cur.getOrElse(default)
      def keyMap(s:Pars#Key):Proc#Key = s
      def valMap(s:Pars#Value):Proc#Value = s
      override def errHandler(p:Pars#BaseImpl):PartialFunction[Throwable,Unit] = {
        case e => println(e.getMessage)
                  e.printStackTrace
                  super.errHandler(p)
      }
      override def update                                            = eCtx.update
      override def converters                                        = eCtx.converters
      override def spawn(i:Binder#I)                                 = eCtx.spawn(i)
      override def merge(cur:Traversable[Any],read:Traversable[Any]) = eCtx.merge(cur,read)
      override def converters(key:String)                            = eCtx.converters(key)
      override def dataActor(cz:Class[_],name:String)                = eCtx.dataActor(cz,name)
    }
  }
  
  //a generic context that works with any parser for a string processor and ObjectMotor.ctx
  def userCtx(out:PrintWriter) = {
    type P0 = ParserBuilder {type Value=String; type Key=String}
    type M0 = ObjectMotor.ctx.type
    val default : ObjectMotor.EltCtx = null
    new Defaults[P0,M0](r,default) with CtxCore.UsrCtx[P0,M0] {
      protected def apply(elt:Proc#Elt,tree:utils.Tree[String,ObjectMotor.EltCtx]):EltCtx = new EltCtx(elt, tree)
      class EltCtx(elt:Proc#Elt,tree:utils.Tree[String,ObjectMotor.EltCtx]) extends super[Defaults].EltCtxBase(elt,tree) with super[UsrCtx].EltCtxBase with ObjectMotor.CtxFullInfer[Pars]
    }
  }
  
  val p = new parsers.Struct(256,40,false)
  
  def baseTest(out:PrintWriter,on:AnyRef) = {
    import ObjectMotor.ctx
    val buf = new java.io.StringWriter
    val m = ctx(on)
    val r=run(p,m)(userCtx(new PrintWriter(buf)),_(ClassContext(on.getClass)),_.read(load("objDepthTest.txt"), "UTF-8"))._2
    out.println(on)
    out.print(buf)
  }
    
  /** Test to verify that an object is correctly filled up ; it tests most cases and ends up with some deep nesting */
  @Test class ObjDepthTest extends StandardTester {
    def apply(file:Solver,out:PrintWriter):Unit = baseTest(out,new L0)
  }

}

