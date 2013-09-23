package loader.reflect


import java.lang.reflect.{Field,Method,Type}
import loader.InternalLoaderException

/** Class used to hide reflexive calls under a common interface.
 */
abstract class FldActor {
  def tagName:String
  def set(on:AnyRef,v:Any):Unit   //the method that sets 'name' in on
  def get(from:AnyRef):Any        //the method that gets 'name' from on  
  def expected:Type               //expected class
}
object FldActor {
  /** Finds the class element most closely matching the given canonical name in the given class.
   *  @param clzz, the class to search
   *  @param name, the name to find
   *  @param accept, when true, return a DummyClassElement that accepts anything but does nothing!
   *  @return the appropriate ClassElt
   */
  def apply(clzz:Class[_],name:String):FldActor = {
    def findMth(nm:String,list:Array[Method]):Method = {
      for (m <- list) if (m.getParameterTypes.length==1 && m.getName==nm) return m
      return null
    }
    def mth(n:String):Method = {
      findMth(n,clzz.getDeclaredMethods) match {
        case null => findMth(n,clzz.getMethods)
        case m    => m
      }
    }
    def fld(n:String):Field = {
      val x = try { clzz.getDeclaredField(n) } catch { case _:Throwable => null }
      if (x!=null) return x
      val y = try { clzz.getField(n) } catch { case _:Throwable => null }
      if (y!=null) return y
      null
    }
    if (clzz==null || name.length==0)
      return null
    fld(name) match { //local fields
      case null => mth(name+"_$eq") match { //scala method
        case null => mth("set"+name.charAt(0).toUpper+name.substring(1)) match { //java beans
          case null => mth(name) match {
            case null => if (false) DummyElt  //XXX check this
                         else throw new InternalLoaderException(s"failed to match <${name}> on a field/method in ${clzz.getName}")
            case x => new MethodElt(x)
          }
          case x => new BeanElt(x)
        }
        case x => new ScalaElt(x)
      }
      case x => new FldElt(x)
    }
  }
  final private class FldElt(f:Field) extends FldActor {
    val tagName = f.getName
    f.setAccessible(true)
    def set(on:AnyRef,a:Any):Unit = f.set(on,a)
    def get(on:AnyRef):Any = f.get(on)
    def expected = f.getGenericType
    override def toString = s"field ${f}"
  }
  final private class BeanElt(set:Method) extends FldActor {
    set.setAccessible(true)
    val tagName = set.getName.charAt(3).toLower+set.getName.substring(4)
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
    override def toString = s"bean ${tagName}"
  }
  final private class ScalaElt(set:Method) extends FldActor {
    set.setAccessible(true)
    val tagName = set.getName.substring(0, set.getName.length-4)
    private val get = {
      val g = try { set.getDeclaringClass.getDeclaredMethod(tagName)
                  } catch { case _:Throwable => try { set.getDeclaringClass.getMethod(tagName)
                                                    } catch { case _:Throwable => null } }
      if (g!=null) g.setAccessible(true)
      g
    }
    def set(on:AnyRef,a:Any):Unit = set.invoke(on,a.asInstanceOf[AnyRef])
    def get(on:AnyRef):Any = if (get!=null) get.invoke(on) else null
    def expected = set.getGenericParameterTypes()(0)
    override def toString = s"scala field ${tagName}"
  }
  final private class MethodElt(set:Method) extends FldActor {
    set.setAccessible(true)
    val tagName = set.getName
    def set(on:AnyRef,a:Any):Unit = set.invoke(on,a.asInstanceOf[AnyRef])
    def get(on:AnyRef):Any = null
    def expected = set.getGenericParameterTypes()(0)
    override def toString = s"method ${tagName}"
  }
  final private object DummyElt extends FldActor {
    val tagName = null
    def set(on:AnyRef,a:Any):Unit = ()
    def get(on:AnyRef):Any = null
    def expected = classOf[AnyRef]
    override def toString = s"dummy field"
  }
  final def checkPrimitive(p1:Class[_],p2:Class[_]):Boolean = {
    if (p1 eq p2)                       return true
    if (!p1.isPrimitive)                return checkPrimitive(p2,p1)
    if (p1 eq java.lang.Integer.TYPE)   return p2 eq classOf[java.lang.Integer]
    if (p1 eq java.lang.Boolean.TYPE)   return p2 eq classOf[java.lang.Boolean]
    if (p1 eq java.lang.Character.TYPE) return p2 eq classOf[java.lang.Character]
    if (p1 eq java.lang.Float.TYPE)     return p2 eq classOf[java.lang.Float]
    if (p1 eq java.lang.Double.TYPE)    return p2 eq classOf[java.lang.Double]
    if (p1 eq java.lang.Short.TYPE)     return p2 eq classOf[java.lang.Short]
    if (p1 eq java.lang.Byte.TYPE)      return p2 eq classOf[java.lang.Byte]
    false
  }
}
