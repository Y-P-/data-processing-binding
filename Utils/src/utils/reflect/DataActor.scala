package utils.reflect


import java.lang.reflect.{Field,Method,Type}
import scala.annotation.tailrec
import Reflect._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

abstract class DataActor {
  def name:String                             //'name' for the data (usually the field name, could be a method name)
  def set(on:AnyRef,v:Any):Unit               //the method that sets 'name' in on
  def get(from:AnyRef):Any                    //the method that gets 'name' from on  
  def expected:Type                           //expected class
  def underlying = findClass[Any](expected)   //the expected java class
}

object DataActor {
  private final def testMethod(m:Method,name:String) = m.getName==name && m.getParameterTypes.length==1
  
  implicit def apply(cz:Class[_]):String=>Option[DataActor] = (s:String)=>apply(cz,s,"bsfm")
  
  //utility for apply below: matches a character and a reflexive setter
  protected[this] def get(cz:RichClass[_],name:String,x:Char) = {
    lazy val m = cz.methods
    lazy val f = cz.fields
    x match {
      case 'b' => m.find (testMethod(_,"set"+name.charAt(0).toUpper+name.substring(1)))  .map(new BeanElt(_))
      case 's' => m.find (testMethod(_,name+"_$eq"))                                     .map(new ScalaElt(_))
      case 'f' => f.find (_.getName==name)                                               .map(new FldElt(_))
      case 'm' => m.find (testMethod(_,name))                                            .map(new MethodElt(_))
      case  c  => throw new IllegalArgumentException(s"the choice for reflexive affectation must be in b (bean), s (scala), f (field), m (method); any other character is an error: $c")
    }
  }
  //recursive call for following the defined order
  @tailrec protected[this] def apply(cz:RichClass[_],name:String,order:String,i:Int):Option[DataActor] = {
    if (i>=order.length) return None
    get(cz,name,order.charAt(i)) match {
      case None      => apply(cz,name,order,i+1)
      case x:Some[_] => x
    }
  }
  /** Finds the class element most closely matching the given canonical name in the given class.
   *  @param clzz, the class to search
   *  @param name, the name to find
   *  @param order, the preferred order to match. e.g. "bf" with name 'aa' would prefer the method setAa rather than field aa, and ignore scala aa_$eq method or any aa method
   *  @return the appropriate ClassElt
   */
  def apply(cz:RichClass[_],name:String,order:String):Option[DataActor] = apply(cz,name,order,0)
  
  final class FldElt(f:Field) extends DataActor {
    val name = f.getName
    f.setAccessible(true)
    def set(on:AnyRef,a:Any):Unit = f.set(on,a)
    def get(on:AnyRef):Any        = f.get(on)
    def expected                  = f.getGenericType
    override def toString         = s"field ${f}"
  }
  final class BeanElt(set:Method) extends DataActor {
    set.setAccessible(true)
    val name = set.getName.charAt(3).toLower+set.getName.substring(4)
    private val get = {
      val nm = set.getName.substring(3)
      val g = try { set.getDeclaringClass.getDeclaredMethod("get"+nm)
                  } catch { case _:Throwable => try { set.getDeclaringClass.getMethod("get"+nm)
                                                    } catch { case _:Throwable => null } }
      if (g!=null) g.setAccessible(true)
      g
    }
    def set(on:AnyRef,a:Any):Unit = set.invoke(on,a.asInstanceOf[AnyRef])
    def get(on:AnyRef):Any = if (get!=null) get.invoke(on) else null
    def expected = set.getGenericParameterTypes()(0)
    override def toString = s"bean ${name}"
  }
  final class ScalaElt(set:Method) extends DataActor {
    set.setAccessible(true)
    val name = set.getName.substring(0, set.getName.length-4)
    private val get = {
      val g = try { set.getDeclaringClass.getDeclaredMethod(name)
                  } catch { case _:Throwable => try { set.getDeclaringClass.getMethod(name)
                                                    } catch { case _:Throwable => null } }
      if (g!=null) g.setAccessible(true)
      g
    }
    def set(on:AnyRef,a:Any):Unit = set.invoke(on,a.asInstanceOf[AnyRef])
    def get(on:AnyRef):Any        = if (get!=null) get.invoke(on) else throw new NotImplementedException
    def expected                  = set.getGenericParameterTypes()(0)
    override def toString         = s"scala field ${name}"
  }
  final class MethodElt(set:Method) extends DataActor {
    set.setAccessible(true)
    val name = set.getName
    def set(on:AnyRef,a:Any):Unit = set.invoke(on,a.asInstanceOf[AnyRef])
    def get(on:AnyRef):Any        = null
    def expected                  = set.getGenericParameterTypes()(0)
    override def toString         = s"method ${name}"
  }
}