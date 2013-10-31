package loader.reflect

import java.lang.reflect.{Field,Method,Type,ParameterizedType,WildcardType,GenericArrayType,Constructor,TypeVariable}
import scala.reflect.ClassTag
import loader.{Element,Assoc,InternalLoaderException,NamedField,Context,Named}
import loader.core.exceptions.{DynamicInvocation,DynamicConstructor}

object U {
/*
 * XXX temporary stopgap
 */
class StringConverter[U] {
  def canConvert(c:Class[_]):Boolean = false
  def get[U](c:Class[_]):Class[U] = null
}
object StringConverter {
  def canConvert(c:Class[_]):Boolean = false
  def get[U](c:Class[_]):StringConverter[U] = null
}
}

/**
 * A class used to describe the steps necessary to go from a tag to it's final destination.
 */
protected[loader] final class Analyze protected (
       czL:Class[_<:AnyRef],                       //the loadable used for that tag
       czF:Class[_],                               //the final class (item class, not container class)
       czLBd:Constructor[_<:AnyRef],               //the constructor for czL
       czColl:Class[_],                            //the collection class, if any
       czS:Class[_],                               //the temporary storage class if any
       cvF:U.StringConverter[Any],                   //the converter to use if any
       czFNamed:Class[_]                           //the built item encapsulated in a NamedField object ; possibly null
      ) {
  def newLoadable                                 = czLBd.newInstance()
  def buildStack(n:Int)                           = if (czS!=null) new Analyze.Stack(czS,czColl,n) else new Analyze.Stack(classOf[Null],classOf[Null],0)
  def getLoadableClass                            = czL
  def getExpectedClass                            = if (czFNamed!=null) czFNamed else czS
  def isNamed                                     = czFNamed!=null
  def isCollection                                = czColl!=null
  def getConverter                                = if (cvF!=null) Some(cvF) else None
  /**
   * Manages containers for identical fields (tags containing lists or sequences of 'identical' tags).
   * @param fd the contained field definition
   */
  final def seq(fd0:Context#FieldMapping) = new loader.commons.Seq {
    val fd = fd0
    //create a temporary buffer
    protected val buf = buildStack(100)
    //pushes a value into the buffer ; we are not supposed to accept null
    def push(value:Any) = {
      if (value==null)
        throw new InternalLoaderException("invalid 'null' value in a list/sequence")
      try { buf+=value } catch { case e:ArrayStoreException => throw new ArrayStoreException(s"received ${value.getClass}, expected ${buf.getEltType}") }
    }
    //terminates by returning the expected container and clears the temporary buffer
    def recover = buf.buildCollection
    //current size of the stack significant content
    def size = buf.length
  }
}

/**
 * Utilities for reflection.
 * This is the factory for building the Analyze class.
 *
 * Three classes are involved during a tag analysis.
 * - the loadable used by the inner analyzer (czLoad)
 * - the result from this loadable as provided by the end call (czTemp)
 * - the method parameter or field type that will use this result (czFinal)
 * All three classes can be equal (a common occurrence is when the tag is a simple structure.)
 * They can also all be different ; a common occurrence is when the final result is a collection of any primitive type.
 * 
 * Most of the time, most of these fields can be found by reflection.
 * - czFinal is found by examining the field/method that will process czTemp
 * - czLoad may be equal to czFinal or czTemp if either class is Loadable[Any][Any]
 * - czTemp is found by examining the result type of the end method in the czLoad class
 * When czTemp and czFinal are not equal, rules are applied to attempt an implicit conversion:
 * - if czFinal is a collection (in a general sense, arrays included), then the contained type is instead used as czFinal and these rules are again applied.
 * - if czTemp is Class[Assoc], then czTemp will be processed as a map element
 * - a conversion using the Converters class will be attempted
 * - if not found, we have an error 
 */
object Analyze {
  import scala.language.implicitConversions

    
  final protected def asX[X:ClassTag](clzz:Class[_], orElse: => Class[_<: X]):Class[X] = (if (isX[X](clzz)) clzz else orElse).asInstanceOf[Class[X]]
  final protected def isX[X:ClassTag](clzz:Class[_]):Boolean = clzz!=null && implicitly[ClassTag[X]].runtimeClass.isAssignableFrom(clzz)
  final protected def isX[X:ClassTag](typ:Type):Boolean      = isX[X](typ.asInstanceOf[Class[_]])
  final protected def isX[X:ClassTag](fld:Field):Boolean     = isX[X](fld.getType)
        
    
  /** put in a method otherwise compiler crash */
  final protected def getConstructor[X](cz:Class[X]):Constructor[X] = {
    if (cz==null) return null
    cz.getDeclaredConstructors.find(_.getParameterTypes.length==0) match {
	    case Some(c) => c.setAccessible(true); c.asInstanceOf[Constructor[X]]
	    case None    => throw new InternalLoaderException(
	      s"No suitable contructor found for $cz (constructor with no argument ; check the type is not an interface or abstract.) "+
	       "This error may happen for collections without a default constructor ; it may also happen when a default conversion is not found")
	  }
  }
  
  /** actual class for a given type */
  implicit def getClass(gType:Type):Class[_] = gType match {  //exhaustive check
    case null                => null
    case c:Class[_]          => c
    case g:GenericArrayType  => java.lang.reflect.Array.newInstance(g.getGenericComponentType,0).getClass
    case p:ParameterizedType => p.getRawType
    case t:TypeVariable[_]   => null                   //dynamic type
    case w:WildcardType      => w.getUpperBounds()(0)
  }
  
  /**
   * retrieves information on a field that returns a collection.
   * - element type stored
   * - method to build the collection from an Int (which might not get used)
   * This uses generics ; if you don't use generics, this won't work!
   * It will work for simple declarations of these kind:
   * - Coll[X]   X is the element stored in the collection ; only one param expected.
   * - Map[K,X]  X is the element stored in the map, K it's key type. K is ignored.
   * @param gType  : the type under scrutiny
   * @return Tuple (is a map, element type, is NamedField, collection builder)
   */
  final protected def infoType(gType:Type,isList:Boolean,isSeq:Boolean):(Boolean,Class[_],Boolean,Class[_]) = {
    (isList||isSeq,gType) match {
      case (false,p)                  => singleInfoType(p)
      case (true,p:ParameterizedType) => checkNamed(p) match {
        case (t,false) => collInfoType(p)
        case (t,true)  => val r = infoType(t,isList,isSeq)
                          if (isList) (r._1,r._2,true,r._4)
                          else        throw new IllegalStateException("Sequences cannot be in a NamedField")
      }
      case (true,p)    => arrayInfoType(p)
    }
  }
  
  /** checking the presence of a NamedField ; returns the actual element type and an indicator (true if NamedField requested) */
  def checkNamed(gType:Type):(Type,Boolean) = {
    gType match {
      case p:ParameterizedType if p.getRawType==classOf[NamedField[_]] => (p.getActualTypeArguments.apply(0),true)
      case _                                                           => (gType,false)
    }
  }
  
  /** analyzing for simple fields (i.e. TagField) */
  private def singleInfoType(gType:Type):(Boolean,Class[_],Boolean,Class[_]) = {
    val r = checkNamed(gType)
    (false, r._1, r._2, null)
  }
  
  /** analyzing for seq/list fields using arrays */
  private def arrayInfoType(gType:Type):(Boolean,Class[_],Boolean,Class[_]) = {
    val e = gType match {
      case g:GenericArrayType      => g.getGenericComponentType
      case a:Class[_] if a.isArray => a.getComponentType
      case _                       => throw new InternalLoaderException("An array declaration was expected")
    }
    val r = checkNamed(e)
    (false, r._1, r._2, gType)
  }
  
  /** analyzing for seq/list fields using collections */
  private def collInfoType(gType:ParameterizedType):(Boolean,Class[_],Boolean,Class[_]) = {
    checkNamed(gType) match {
      case (t,false) => 
        val args = gType.getActualTypeArguments
        val e = args(args.length-1) match {  //last param = item class
          case w:WildcardType      => if (w.getUpperBounds.length!=1 || w.getLowerBounds.length!=0) throw new InternalLoaderException("This utility supports wildcard only when it has exactly one upper bound")
                                      w.getUpperBounds.apply(0)
          case x                   => x
        }
        val r = checkNamed(e)
        (args.length>1,r._1,r._2,gType)
      case (_,true) => throw new InternalLoaderException(s"failed to recognize a collection on $gType")
    }
  }
  
  //Unused
  /*
  implicit class ObjCheck(o:Any) {
    def asA[X:ClassTag]:Option[X]     = if (implicitly[ClassTag[X]].runtimeClass.isAssignableFrom(o.getClass)) Some(o.asInstanceOf[X]) else None
    def isA[X:ClassTag]:Boolean       = o.getClass <:< implicitly[ClassTag[X]].runtimeClass
    def <:<[X:ClassTag](o1:X):Boolean = o.getClass <:< o1.getClass
  }
  implicit class ClassCheck(clzz:Class[_]) {
    def <:<(clzz1:Class[_]):Boolean = clzz1.isAssignableFrom(clzz)
  }
  */
  /**
   * Type analyzer.
   * 
   * 1) Analyze tFinal
   *    - simple class      : (null, F, null)
   *    - generic Coll[F]   : (null, F, builder)
   *    - generic Coll[K,F] : (K,    F, builder)
   *    with builder a method (Int) => Coll[F] ou (Int) => Coll[K,F]
   * 2) Find loader class: loader
   *    - if czLoad given, use it
   *    - if F<:Loadable[Any][Any], use it
   *    - use BaseField
   * 3) Examine loader.end method return type
   *    - simple class : (F', null)
   *    - Assoc        : (F', K')
   * 4) Find implicit converter for F and K: cvF, cvK (identical algorithms)
   *    - if (F' <: F) : identity
   *    - if Converters.get(F',F) : use it
   *    - otherwise : identity
   * 5) Find internal storage format: intern
   *    - if K', Assoc(K',F')
   *    - otherwise F'
   * 6) Build return: (loader,cvF,cvK,intern)
   *    
   * 
   * @param czLoad:Class[_<:Loadable[Any]]   Loadable[Any] to use (if null, find it)
   * @param gFinal                           Type at the receiving end (field/method parameter)
   * @param single                           true if not dealing with a collection
   */
  def apply(gFinal:Type,czLoad:Class[_<:AnyRef],isList:Boolean,isSeq:Boolean):Analyze = {
    import scala.language.existentials
    val (isMap,czF0,isNamed,builder) = infoType(gFinal,isList,isSeq)       //gets the key type (map), final expected type, collection builder (collections)
    val czF = if (czF0!=null && czF0!=classOf[AnyRef]) czF0 else throw new DynamicInvocation
    //impossible to staticaly detect the kind of built object ; rely on dynamic binding ; INFO: beware scala autoboxed primitive:we cannot detect them in generics and get Object instead!
    val czL = (if (czLoad!=null) czLoad else if (U.StringConverter.canConvert(czF)) null else czF).asInstanceOf[Class[_<:AnyRef]] //null if we are dealing with a converter
    val cons = try { getConstructor(czL) } catch { case e:Exception=> throw new DynamicConstructor }
    val czS = if (isMap) classOf[Assoc[_,_]] else if (isNamed && !isList) classOf[NamedField[_]] else czF //map elts are internally stored as Assoc, named elt are kept as NamedField, others are kept as their final type
    val cv = if (czL==null || classOf[U.StringConverter[_]].isAssignableFrom(czL) || czL!=czF)          //terminal fields using converters, not loaders...
               if (czF0==null) null else U.StringConverter.get[Any](czF)
             else
               null
    new Analyze(czL,czF,cons,builder,czS,cv,if (isNamed) czF else null)
  }
  def apply(fld:Field,czLoad:Class[_<:AnyRef],isList:Boolean,isSeq:Boolean):Analyze = {
    Analyze(fld.getGenericType,czLoad,isList,isSeq)
  }
  def apply(mtd:Method,czLoad:Class[_<:AnyRef],isList:Boolean,isSeq:Boolean):Analyze = {
    Analyze(mtd.getGenericParameterTypes()(0),czLoad,isList,isSeq)
  }
  //Builder for the top class
  def apply(czLoad:Class[_<:AnyRef]):Analyze = {
    val c = try { getConstructor(czLoad) } catch { case _:Throwable => null }  //acceptable, but includes will not get loaded.
    new Analyze(czLoad,null,c,null,null,null,null)
  }

  //TODO ?: improve perf by moving reflexion out. Not worth it though (collection building doesn't happen that often).
  class Stack[+X](cx:Class[X],czColl:Class[_],sz:Int=200) extends utils.Stack(cx,sz) {
    def buildCollection:AnyRef = {
      val r = if (czColl.isArray) {
        val r = java.lang.reflect.Array.newInstance(czColl.asInstanceOf[Class[Array[_]]].getComponentType, length).asInstanceOf[Array[X]]
        arrayCopy(r)
        r
      } else if (classOf[scala.collection.MapLike[_,_,_]].isAssignableFrom(czColl)) {
        val builder = czColl.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,Any,_]].apply
        builder.sizeHint(length)
        for (x <- this) builder += { val a=x.asInstanceOf[Assoc[Any,Any]]; (a.key,a.value) }
        builder.result.asInstanceOf[AnyRef]
      } else if (classOf[scala.collection.Iterable[_]].isAssignableFrom(czColl)) {
        val builder = czColl.getMethod("canBuildFrom").invoke(null).asInstanceOf[scala.collection.generic.CanBuildFrom[_,X,_]].apply
        builder.sizeHint(length)
        for (x <- this) builder += x
        builder.result.asInstanceOf[AnyRef]
      } else if (classOf[java.util.Map[_,_]].isAssignableFrom(czColl)) {
        val r = czColl.newInstance.asInstanceOf[java.util.Map[Any,Any]]
        for (x <- this) { val a=x.asInstanceOf[Assoc[Any,Any]]; r.put(a.key,a.value); }
        r
      } else if (classOf[java.util.Collection[_]].isAssignableFrom(czColl)) {
        val r = czColl.newInstance.asInstanceOf[java.util.Collection[X]]
        for (x <- this) r.add(x)
        r
      }
      else
        throw new InternalLoaderException("Unsupported container class")
      clear
      r
    }
  }
  /** loops on a generalized collection */
  def asIterable[X](col:AnyRef) = new Traversable[X] {
    def foreach[U](body:(X)=>U):Unit = {
      import collection.JavaConversions._
      val cx = col.getClass
      if (cx.isArray)                                                         col.asInstanceOf[Array[X]].foreach(body)
      else if (classOf[scala.collection.MapLike[_,_,_]].isAssignableFrom(cx)) col.asInstanceOf[scala.collection.MapLike[_,X,_]].foreach(x=>body(x._2))
      else if (classOf[scala.collection.Iterable[_]].isAssignableFrom(cx))    col.asInstanceOf[scala.collection.Iterable[X]].foreach(body)
      else if (classOf[java.util.Map[_,_]].isAssignableFrom(cx))              col.asInstanceOf[java.util.Map[_,X]].foreach(x=>body(x._2))
      else if (classOf[java.util.Collection[_]].isAssignableFrom(cx))         col.asInstanceOf[java.util.Collection[X]].foreach(body)
      else throw new InternalLoaderException(s"Unsupported container class : $cx")
    } 
  }
  /** loops on a generalized map */
  def asMapIterable[X,Y](col:AnyRef) = new Traversable[(X,Y)] {
    def foreach[U](body:((X,Y))=>U):Unit = {
      import collection.JavaConversions._
      val cx = col.getClass
      if      (classOf[scala.collection.MapLike[_,_,_]].isAssignableFrom(cx)) col.asInstanceOf[scala.collection.MapLike[X,Y,_]].foreach(x=>body(x))
      else if (classOf[java.util.Map[_,_]].isAssignableFrom(cx))              col.asInstanceOf[java.util.Map[X,Y]].foreach(x=>body(x))
      else throw new InternalLoaderException(s"Unsupported map class : $cx")
    } 
  }
  /** Checks if cx is a 'collection' class in the broadest meaning (java/scala Map/Iterable/Aray)
   *  @returns (isList,isMap) */
  def isCollection(cx:Type):(Boolean,Boolean) = {
    val c:Class[_] = cx
    if (c==null)                                                      return (false,false)
    if (c.isArray)                                                    return (true,false)
    if (classOf[scala.collection.MapLike[_,_,_]].isAssignableFrom(c)) return (true,true)
    if (classOf[scala.collection.Iterable[_]].isAssignableFrom(c))    return (true,false)
    if (classOf[java.util.Map[_,_]].isAssignableFrom(c))              return (true,true)
    if (classOf[java.util.Collection[_]].isAssignableFrom(c))         return (true,false)
    return (false,false)
  }
}

