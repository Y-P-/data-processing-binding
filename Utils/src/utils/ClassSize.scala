package utils

import scala.reflect.ClassTag

/**
 * Used to measure the actual size of any given class with a default constructor.
 */
object ClassSize {

  //24 bits
  class BaseField1(val pos:Int, val lg:Int=0, val data:Int=0, val lgData:Int=0) {
    def this() = this(0)
  }
  //16 bits
  class BaseField2(val pos:Int, val lg:Byte=0, val data:Byte=0, val lgData:Short=0) {
    def this() = this(0)
  }
  class StructField1(val pos:Int, val lg:Byte=0, val data:Byte=0, val close:Int=0, var fld:Array[Null]) {
    def this() = { this(0,fld=Array(null,null,null,null)) }
  }
  class StructField2(val pos:Int, val lg:Byte=0, val data:Byte=0, val close:Int=0, var fld:Array[Null], var vals:Array[Long]) {
    def this() = { this(0,fld=Array(null),vals=Array[Long](4)) }
  }
  
  def gc() = Runtime.getRuntime.gc
  def free = Runtime.getRuntime.freeMemory
  def total = Runtime.getRuntime.totalMemory
  
  def measure[K:ClassTag](nb:Int=100000):Array[K] = {
    val m = implicitly[ClassTag[K]]
    val t = new Array[K](nb)
    val k0 = m.runtimeClass.newInstance.asInstanceOf[K]
    var i = 0
    total-free
    gc
    val used0 = total-free
    do {
      t(i) = m.runtimeClass.newInstance.asInstanceOf[K]
      i+=1
    } while (i<nb)
    gc
    val used1 = total-free
    val r = (used1-used0)/nb
    println("Class "+m.runtimeClass.getSimpleName+" uses approximatively "+r+" bits")
    t
  }
  
  def main(args: Array[String]): Unit = {
    measure[StructField1]()
  }

}