package parser.px.analyzers.structured
import scala.collection.immutable.Stack

/**
 * Defines common methods for Field like objects.
 * This doesn't preclude any specific implementation for the actual class.
 */
trait Field {
  //method used to retrive the name the first time
  //protected def nameAsStr:String
  /** the name of the field ; it is a String ; "" for anonumous fields */
  def name:String
  /** the value of the field ; exception if a container */
  def value:String
  /** String that's printed when entering the field */
  def leftString:String
  /** String that's printed when exiting the field */
  def rightString:String
  /** Tells whether the field has this name */
  final def isName(name:String):Boolean = name == this.name

  implicit def toInt     = Integer.parseInt(value)
  implicit def toDouble  = java.lang.Double.parseDouble(value)
  implicit def toBool    = if (value.equals("yes")) true else if (value.equals("no")) false else throw new NumberFormatException("not a boolean")

  /**
   * Methods to access fields ; for leaves, these will throw exceptions.
   */
  def select(name:String):Seq[Field]

  /**
   * Methods to edit leaves ; for containers, these will throw exceptions.
   * Fields do not have to implement any or all of these if they are not appropriate to the implementation.
   */
  def :=(newValue:String):Unit  = throw new UnsupportedOperationException
  def :=(newValues:Field*):Unit = throw new UnsupportedOperationException
  def :=(newValue:Int):Unit     = :=(newValue.toString)
  def :=(newValue:Boolean):Unit = :=(if(newValue)"yes"else"no")
  def :=(newValue:Double):Unit  = :=(newValue.toString)

  /**
   * Methods to edit containers.
   * These methods will fail if the field is Leaf.
   * Fields do not have to implement any or all of these if they are not appropriate to the implementation.
   * You don't have any reason to call these methods directly : Binders provide a nice wrapper.
   */
  /** Inserts a field at the old field position ; the old field (and following) are moved */
  def insertAt(old:Field,fld:Field):Unit = throw new UnsupportedOperationException
  /** Inserts a field after the old field ; the next fields are moved */
  def insertAfter(old:Field,fld:Field):Unit = throw new UnsupportedOperationException
  /** Replaces the old field */
  def replace(old:Field,fld:Field):Unit = throw new UnsupportedOperationException
  /** Deletes the old field */
  def delete(old:Field):Unit = throw new UnsupportedOperationException

  /**
   * Walks recursively through all contained fields.
   * This is a "deep" walk, that does explore Containers recursively.
   * Applies f (as defined by the call) on all elements (nested).
   * The second entry in f is the recurse token ; it's position indicates the
   * place in f where you want the recursion to occur.
   * See toString below to see deep in action.
   */
  def deep[R](f:(Field, =>Unit)=>R):R = f(this,Unit)
  /**
   * Similar to deep, but keeps the Field call stack, which enables a Field
   * to access parents when processing f.
   */
  def deep2[R](stack:Stack[Field])(f:(Field, =>Unit, Stack[Field])=>R):R = f(this,Unit,stack)
  /**
   * Similar, but folds over a value of type R.
   */
  def deep3[R](r:R, f:(R, Field, (R)=>R)=>R):R = f(r,this,identity)
  /**
   * The common String representation for the field.
   */
  override def toString = leftString+rightString
  /**
   * A method designed to be used with deep2 in order to (pretty) print a Field and all it's children.
   * Use it as in: <code>field.deep2() { (f,recurse,stack) => f.printer(recurse,stack) }</code>
   */
  protected def printer(use:(String)=>Unit, sep:(Int)=>String, recurse: => Unit, stack:Stack[Field]) = {
      use(sep(stack.size-1))
      use(leftString)
      recurse
      use(rightString)
  }
  /**
   * 'Prints' the field (according to what you do inside 'use'.)
   * Indents if indent is true. If you don't, the result holds on one line.
   */
  def printer(use:(String)=>Unit,indent:Boolean=true):Unit = {
      val sep = if (indent) (n:Int)=>Field.indent(n) else (n:Int)=>" "
      deep2(new Stack[Field]) { (f,recurse,stack) => f.printer(use,sep,recurse,stack) }
  }
  /** Conversion of that field to Container ; useful when looking for embedded fields */
  def toStruct = asInstanceOf[Container]
}

object Field {
  protected val indentations=Array("\n","\n  ","\n    ","\n      ","\n        ","\n          ","\n            ","\n              ","\n                ")
  protected def indent(n:Int) = try { indentations(n) } catch { case _:Throwable => val sb=new StringBuffer("\n"); var i=n; if (n>0) do { sb.append("  "); i-=1 } while (i>0); sb.toString }
}