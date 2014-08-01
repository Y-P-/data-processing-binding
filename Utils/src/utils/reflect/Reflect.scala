package utils.reflect

import java.lang.reflect.{Method,Field,Constructor,AccessibleObject,Modifier,Type,GenericArrayType,ParameterizedType,TypeVariable,WildcardType}
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe.Symbol
import scala.reflect.runtime.universe.newTermName

object Reflect {
  
  /** underlying class for a given type.
   *  Class             => that object
   *  GenericArrayType  => the underlying array class (stripped of genericity)
   *  ParameterizedType => the underlying class (stripped of genericity)
   *  TypeVariable      => is unexpected
   *  WildcardType      => is unexpected
   */
  implicit def findClass[U](gType:Type):Class[_<:U] = (gType match {  //exhaustive check
    case c:Class[_]          => c
    case g:GenericArrayType  => java.lang.reflect.Array.newInstance(g.getGenericComponentType,0).getClass
    case p:ParameterizedType => p.getRawType
    case t:TypeVariable[_]   => throw new IllegalStateException(s"Real types are expected ; found $t")
    case w:WildcardType      => throw new IllegalStateException(s"Non wilcard types are expected ; found $w")
  }).asInstanceOf[Class[_<:U]]
  
  abstract class AccessibleElement {
    type Kind>:Null<:AccessibleObject
    def obj:Kind
    def getModifiers:Int
    def isSynthetic:Boolean
    def isBridge:Boolean
    def getName:String
    def compareTo(m:Kind):Option[Int] 
    def debug = {
      val m = getModifiers
      print(getName)
      if (Modifier.isPublic(m)) print(" public")
      if (Modifier.isPrivate(m)) print(" private")
      if (Modifier.isProtected(m)) print(" protected")
      if (isSynthetic) print(" synthetic")
      if (isBridge) print(" bridge") 
    }
  }
  object AccessibleElement {
    val nonComparable = new Exception { override def fillInStackTrace=this }
    val nonUniqueMin  = new Exception { override def fillInStackTrace=this }
    val s0 = Some(0)
    val sp = Some(1)
    val sm = Some(-1)
    def cmp(c1:Class[_],c2:Class[_]):Option[Int] = {
      val b1 = c1.isAssignableFrom(c2)
      val b2 = c2.isAssignableFrom(c1)
      if (b1) if (b2) s0 else sp
      else    if (b2) sm else None
    }
    /** Compares two lists of classes, to determine if they are compatible, and their
     *  relation order.
     *  @param start, the expected order if any ; if unknown, use None
     *  @param r1 the first class list
     *  @param r2 the second class list
     *  @return s0 if the lists are equal,
     *          sp if r1>r2 (i.e. all classes in r2 are derived from classes in r1)
     *          sm if r1<r2 (i.e. all classes in r1 are derived from classes in r2)
     *          None if the classes cannot be compared
     */
    def cmp(r1:Array[Class[_]],r2:Array[Class[_]],start:Option[Int]):Option[Int] = {
      if (r1.length!=r2.length) return None
      var c=start
      var i:Int=0
      while (i<r1.length) {
        cmp(r1(i),r2(i)) match {
          case None          => return None       //non comparable classes
          case `sp` if c==sm => return None       //comparable, but order inverted from what was seen before
          case `sm` if c==sp => return None       //comparable, but order inverted from what was seen before
          case `s0`          => if (c==None) c=s0 //equal: do nothing
          case r             => c=r               //order can be selected
        }
        i += 1
      }
      c
    }
    final def min[E<:AccessibleObject](l:Array[E]):E = min[E,E](l)(identity)
    
    final def min[E<:AccessibleObject,X](l:Seq[X])(f:X=>E):X = {
      if (l.isEmpty) throw nonComparable
      var min:X = null.asInstanceOf[X]
      var fMin:E = null.asInstanceOf[E]
      for (x <- l) {
        val fX = f(x)
        if (min==null) { min=x; fMin=fX } //first item
        else {
          fX.compareTo(fMin) match {
            case `sm` => min=x; fMin=fX
            case `s0` => throw nonUniqueMin
            case None => throw nonComparable
            case _    => 
          }
        }
      }
      min      
    }
    final implicit def apply[E<:AccessibleObject](obj:E):AccessibleElement { type Kind=E } = (obj match {
      case o:Method         => new MethodX(o)
      case o:Field          => new FieldX(o)
      case o:Constructor[_] => new ConstructorX(o)
    }).asInstanceOf[AccessibleElement { type Kind=E }]
  }
  implicit final class MethodX(val obj:Method) extends AccessibleElement {
    final type Kind        = Method
    final def getModifiers = obj.getModifiers
    final def isSynthetic  = obj.isSynthetic
    final def isBridge     = obj.isBridge
    final def getName      = obj.getName
    final def compareTo(m:Method):Option[Int] = {
      import AccessibleElement._
      if (m==obj) s0
      else cmp(obj.getParameterTypes, m.getParameterTypes, None) //cmp(m.getReturnType,obj.getReturnType)
    }
  }
  implicit final class ConstructorX(val obj:Constructor[_]) extends AccessibleElement {
    final type Kind        = Constructor[_]
    final def getModifiers = obj.getModifiers
    final def isSynthetic  = obj.isSynthetic
    final def isBridge     = false
    final def getName      = obj.getName
    final def compareTo(m:Constructor[_]):Option[Int] = {
      import AccessibleElement._
      if (m==obj) s0
      else cmp(obj.getParameterTypes, m.getParameterTypes, None) //cmp(m.getDeclaringClass,obj.getDeclaringClass)
    }
  }
  implicit final class FieldX(val obj:Field) extends AccessibleElement {
    final type Kind        = Field
    final def getModifiers = obj.getModifiers
    final def isSynthetic  = obj.isSynthetic
    final def isBridge     = false
    final def getName      = obj.getName
    final def compareTo(m:Field):Option[Int] = {
      import AccessibleElement._
      if (m==obj) s0
      else cmp(Array(m.getType,m.getDeclaringClass),Array(obj.getType,obj.getDeclaringClass),None)
    }
  }
    
  /** Provides some Java reflection utilities.
   *  Note that this class doesn't achieve anything close to scala reflection.
   *  It doesn't use any compile time info and is not suitable for general use with generics.
   */
  implicit final class RichClass[+U](val c:Class[_<:U]) {
    final val isFinal = Modifier.isFinal(c.getModifiers())
    //scala singleton associated with this class if appropriate
    def asObject:U = (try {
      val f = c.getDeclaredField("MODULE$")
      val m = f.getModifiers
      if (Modifier.isFinal(m) && Modifier.isStatic(m)) f.get(null) else null
    } catch {
      case _:Throwable => null
    }).asInstanceOf[U]
    //subclass checks
    // !!! Not for use with generic types.
    //     These are JVM checks, not scala Types checks. Two 'unrelated' classes can thus be
    //     found in relation to each other when they are not! e.g. List[Double] and List[Method]
    //     both erase to List and are superficially seen as compatible, even though they obviously
    //     do not share much in common.
    final def <(c:Class[_]):Boolean = c.isAssignableFrom(this.c)
    final def >(c:Class[_]):Boolean = this.c.isAssignableFrom(c)
    final def <(c:RichClass[_]):Boolean = this < c.c
    final def >(c:RichClass[_]):Boolean = this > c.c
    //finds appropriate constructors matching the expected class list, whatever order, expect for the mandatory first classes that must be in the correct order. Generics are out.
    def findConstructor(expected:Array[RichClass[_]],mandatory:Int):Array[_<:(_<:Constructor[_<:U],Array[Int])] =
      Reflect.findConstructor[U](c.getConstructors.asInstanceOf[Array[_<:Constructor[_<:U]]],expected,mandatory)
    //finds the constructor matching the expected class list. Generics are out.
    def findConstructorN(expected:RichClass[_]*):Option[Constructor[_<:U]] =
      Reflect.findConstructor[U](c.getConstructors.asInstanceOf[Array[_<:Constructor[_<:U]]],expected.toArray,expected.length) match {
        case Array()      => None
        case Array((c,_)) => Some(c)
    }
    
    final def printMethods() = methods.foreach(println)
    override def toString       = s"RichClass[${c.getCanonicalName}]"
    override def equals(o:Any)  = if (o.isInstanceOf[RichClass[_]]) o.asInstanceOf[RichClass[_]].c eq this.c else false
    override def hashCode       = c.hashCode
    
    //standard way to retrieve useful methods,fields and construtors
    //this will retrieve all public methods from the class and superclasses, and all methods from the class itself (protected or private)
    //a method is present only once
    //synthetic methods are excluded
    def methods      = (c.getDeclaredMethods.filter(m => !Modifier.isPublic(m.getModifiers))++c.getMethods).filter(!_.isSynthetic)
    def fields       = (c.getDeclaredFields.filter(m => !Modifier.isPublic(m.getModifiers))++c.getFields).filter(!_.isSynthetic)
    def constructors = (c.getDeclaredConstructors.filter(m => !Modifier.isPublic(m.getModifiers))++c.getConstructors).filter(!_.isSynthetic)
  }
  
  //easy factory
  def ^[U](c:Class[U]) = new RichClass[U](c)
  
  /** Finds the method in an array that is closest to the parameter/return types given.
   *  The parameter checked is the first.
   *  The returned method has the minimal parameter type then maximum return type admissible
   *  Note that this is not intended to deal with complex types (generics most notably.)
   *  @param a, an array of method to check
   *  @param src, the first parameter expected class. Can be null if that is not to be checked.
   *  @param dst, the return class. Can be null if that is not to be checked.
   */
  def reduce(a:Array[Method],src:RichClass[_],dst:RichClass[_]):Array[Method] = {
    var s = src
    val l1 = if (src==null) a else {
      //find minimal class for source
      for (m <- a if dst==null || dst>m.getReturnType()) { val x=m.getParameterTypes()(0); if (s>x) s=x }
      //build sublist with minimal class for source
      for (m <- a if s>m.getParameterTypes()(0)) yield m
    }
    val l = if (l1.length<=1 && dst!=null) l1 else {
      s = null
      //find maximal class for return type
      for (m <- l1) { val x=m.getReturnType(); if (s==null || s<x) s=x }
      //build sublist with maximal class for return type
      for (m <- a if s<m.getReturnType()) yield m
    }
    l
  }
  
  /** Finds the methods (static or not) in src that return dst (or a subclass of).
   *  @param  src, the source class in which we are looking for an appropriate method (possibly static)
   *  @param  dst, the class to return
   *  @param  check, a method that adds additional criterion on a method (such as name...)
   *  @return the list of matching Converter
   *  @throws NoSuchMethodException if no method or more than one method is found matching
   */
  def find[U<:AnyRef,V](src:RichClass[U],dst:RichClass[V],check:(Method)=>Boolean):Array[Method] =
    reduce(src.methods.filter(m => check(m) && dst>m.getReturnType),src,dst)
    
  ////////////////////////////////////////////////////////////////////////////////////////////////  
  // The following methods are useful to deal with reflexion around variable list of parameters //
  ////////////////////////////////////////////////////////////////////////////////////////////////  
    
  /** Matches an incoming list of classes against an expected list of classes. Classes must be unrelated or the match will be unpredictable.
   *  @param expected, the list of possible classes
   *  @param incoming, the list of found classes ; they must all be assignable to at most one of the expected classes.
   *                   it is possible to only partially match the incoming list, but the expected list must be fully met.
   *  @param mandatory, the number of elements in expected that must be found in the right order in found
   *  @return an array indicating how indexes in incoming match indexes in expected. null if fails (i.e some incoming elts don't match any expected one)
   */
  def checkParams(expected:Array[Class[_]],incoming:Array[RichClass[_]],mandatory:Int):Array[Int] = {
    if (incoming.length<expected.length) return null
    if (expected.length==0) return if (mandatory==0) new Array[Int](0) else null
    val a = new Array[Int](expected.length)
    //mandatory arguments must be matched at their exact position
    for (i <- 0 until mandatory) if (!(incoming(i)<expected(i))) return null else a(i)=i
    val r = mandatory until incoming.length
    //loop on other expected arguments
    for (i <- mandatory until expected.length) {               
      if (r.find(incoming(_)<expected(i)).map(a(i)=_)==None)   //check if an incoming argument matches and if found, record its index
        return null                                            //if none found, arguments match fails
    }
    a
  }
  /** Builds the actual parameter array from a list of possible parameters, based on the substitution array 'matching' (likely coming
   *  from a call to the previous method)
   */
  def buildParams(possible:Array[AnyRef],matching:Array[Int]):Array[AnyRef] = {
    val a = new Array[AnyRef](matching.length)
    for (i <- 0 until a.length) a(i) = possible(matching(i))
    a
  }
  
  /** restricts 'in' to the Methods that do accept the right kind of parameters */
  def findMethod(in:Array[Method],incoming:Array[RichClass[_]],mandatory:Int):Array[(Method,Array[Int])] =
    for (m <- in; p=checkParams(m.getParameterTypes,incoming,mandatory) if p!=null) yield (m,p)
  /** restricts 'in' to the Constructors that do accept the right kind of parameters */
  def findConstructor[U](in:Array[_<:Constructor[_<:U]],incoming:Array[RichClass[_]],mandatory:Int):Array[(Constructor[_<:U],Array[Int])] =
    for (m <- in; p=checkParams(m.getParameterTypes,incoming,mandatory) if p!=null) yield (m,p)
    
  /** returns true if p1 and p2 represent the same primitive type, Java Boxed or not */
  final def checkPrimitive(p1:Class[_],p2:Class[_]):Boolean = {
    if (p1 eq p2)                       return true
    if (!p1.isPrimitive)                return p2.isPrimitive && checkPrimitive(p2,p1)
    if (p1 eq java.lang.Integer.TYPE)   return p2 eq classOf[java.lang.Integer]
    if (p1 eq java.lang.Boolean.TYPE)   return p2 eq classOf[java.lang.Boolean]
    if (p1 eq java.lang.Character.TYPE) return p2 eq classOf[java.lang.Character]
    if (p1 eq java.lang.Float.TYPE)     return p2 eq classOf[java.lang.Float]
    if (p1 eq java.lang.Double.TYPE)    return p2 eq classOf[java.lang.Double]
    if (p1 eq java.lang.Short.TYPE)     return p2 eq classOf[java.lang.Short]
    if (p1 eq java.lang.Byte.TYPE)      return p2 eq classOf[java.lang.Byte]
    if (p1 eq java.lang.Long.TYPE)      return p2 eq classOf[java.lang.Long]
    false
  }
  
  /** Analyzes a type to determine if it is a collection, and in that case the relevant information.
   * @param t, the type to analyze
   * @param cv, the conversion solver in use
   * @param n, the depth for the analysis (0 is all the way to the bottom of encapsulated seqs/lists)
   * @returns  the actual depth if less than n, then None if the type can be converted or Some(class found)
   */
  def analyzeType(t:java.lang.reflect.Type, cv:ConversionSolver, n:Int):(Int, Option[java.lang.reflect.Type]) =
    cv.collectionSolver(t) match {
      case Some(l) => val x = l.depth(n)
                      val isConvertible = cv.stringSolver(x._2.czElt)
                      (x._1, if (isConvertible==None) Some(x._2.czElt) else None)
      case None    => val isConvertible = cv.stringSolver(t)
                      (0,if (isConvertible==None) Some(t) else None)
    }
  
      //XXX for fun... test on the Scala reflective API sho it is very slow for our requirements
      def copy[T<:AnyRef:scala.reflect.ClassTag](b:T,p1: String):T = {
        import scala.reflect.runtime.{ currentMirror => cm }
        import scala.reflect.runtime.universe._
        val im = cm.reflect(b)
        val ts = im.symbol.typeSignature
        val copySym = ts.member(TermName("copy")).asMethod
        def element(p: Symbol): Any = (im reflectMethod ts.member(p.name).asMethod)()
        val args = for (ps <- copySym.paramLists; p <- ps) yield {
          if (p.name.toString == "p1") p1 else element(p)
        }
        (im reflectMethod copySym)(args: _*).asInstanceOf[T]
      }      
     
}