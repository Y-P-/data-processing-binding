package loader.test
import loader.annotations._
import java.io.StringWriter
import java.io.Writer
import java.lang.Integer
import loader.commons._
import loader.Named
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer


object Data {

  import loader._
  import test._
  import scala.collection.mutable.{HashMap,ListBuffer}
  
  type Element = loader.core.CtxCore#Elt

  @TagStruct
  class Id {
    @TagField(inName="type") var types:Int=_
    @TagField var id:Int=_
    @Convert def tagEnd(l:Element) = id+100000*types
  }

  @TagStruct
  class Encap extends Named {
    @TagField var on:Int=_
    @TagField var off:Int=_
    @Convert def tagEnd(l:Element) = on+off
    override def toString = "on="+on+" off="+off
    def name_=(nm:String) = println(nm)
  }
  @TagStruct
  class Encap2 {
    @TagField var on:Int=_
    @TagField var off:Int=_
    @Convert def tagEnd(l:Element) = on --> off
    override def toString = "on="+on+" off="+off
  }
  @TagStruct
  class GlobalData {
    val cz0 = classOf[Treaty]
    final def cz = cz0
    //@TagSeq(dynamic="cz") var treaty:Array[AnyRef] = _
    @TagSeq(loader=classOf[Treaty]) var treaty:Array[Treaty] = _
    @Convert def tagEnd(l:Element) = this
  }
  
  @TagStruct
  class Treaty extends Named {
    @TagSeq var country:Array[String] = _
    @TagField(inName="{types?}") var types:String=_
    @Convert def tagEnd(l:Element) = this
    def name_=(nm:String) = println(nm)
    def output = {
      val b = new StringBuffer
      b.append("treaty :").append(types)
      if (country!=null) for (c <- country) b.append(" "+c)
      b.toString
    }
  }

  @TagStruct
  class Province {
    @TagField var id:Int=_
    @TagField(check=".*") var goods:String=_            //was ....
    @Convert def tagEnd(l:Element) = id --> goods
  }

  @TagStruct
  class Header extends Named {
    @TagField var tutorial:Boolean=_
    @TagField var set_ai_aggresive:Int=_
    @TagField(inName="saved")
    def saved(v:Boolean) = if (v) println("WAS SAVED") else println("WAS NOT SAVED") 
    @TagSeq(loader=classOf[Id],contiguous=false) var id:Array[Int]=_
    @Convert def tagEnd(l:Element)  = this
    def name_=(nm:String) = println(nm)
  }

  @TagStruct(auditMax=5, fast=true)
  class Top {
    import java.lang.Integer
    //this for serialization tests
    val aax:Long = 5
    val aay:Int = 4
    val date = new java.util.GregorianCalendar(2012,12,12).getTime
    //@TagField
    //var load:loader.Parser.Value = _
    @TagSeq(loader=classOf[Top],min=3)
    var include:Array[Top]=null
    @TagField(loader=classOf[Header])
    protected var header:Header=null
    @TagField
    var test:Int=_
    @TagField(loader=classOf[Encap], valid="3")
    var oye:Integer=_
    @TagList(loader=classOf[Encap], valid="3")
    var encap:ListBuffer[Integer]=_
    @TagList(loader=classOf[Encap2],max=1)
    var encap2:HashMap[Integer,Integer]=_
    @TagSeq(inName="{even.}", check=".*", contiguous=true) //was ....
    var event:Array[String]=_
    @TagList(check="...(.*)",valid="[000,800000]",min=10,max=100)  //was ....(.)?, [7000,8000]
    var history:Array[Int]=_
    @TagField(loader=classOf[GlobalData])
    var globaldata:GlobalData=_
    @TagSeq(inName="{provinc.}",loader=classOf[Province],min=50)
    var province:scala.collection.immutable.HashMap[Integer,String]=_
    @Convert def tagEnd(l:Element):this.type = this
    def write(out:Writer):Unit = {
      def println(s:String) = { out.write(s); out.write("\n"); }
      if (header!=null) println("ai_agg   = "+header.set_ai_aggresive+" "+header.set_ai_aggresive.getClass )
      if (header!=null) println("tutorial = "+header.tutorial)
      if (header!=null) if (header.id!=null) println(s"header.id = ${header.id.toList}")
      if (event!=null)  out.write("event    = "+event.mkString(",")+"\n")
      println("test     = "+test+" "+test.getClass)
      println(s"history  = ${if (history!=null) history.mkString("[",",","]") else "[]"}")
      println("oye      = "+oye)
      if (province!=null) println(province.toString)
      if (encap2!=null)   println(encap2.toString)
      if (globaldata!=null && globaldata.treaty!=null) for (t <- globaldata.treaty) println(t.asInstanceOf[Treaty].output)
      println("encap = "+encap)
      if (include!=null) println(s"${include.length} includes")
      if (include!=null && include.length>=1 && include(0)!=null) include(0).write(out)
      if (include!=null && include.length>=2 && include(1)!=null) include(1).write(out)
      if (include!=null && include.length>=3 && include(2)!=null) include(2).write(out)
    }
    override def toString = {
      val out = new StringWriter
      write(out)
      out.toString
    }
  }
  
  class MyTop extends Top
  
}