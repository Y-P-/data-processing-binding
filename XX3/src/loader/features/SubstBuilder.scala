package loader.features

import scala.collection.Map
import loader.Element

trait SubstBuilder {
  /**
   * Builds a variable substituter.
   * Note: a naming scheme is assumed, that lets easily identify variables.
   *       the objective of this builder is to give the capability to create a custom scheme if necessary.
   * 
   * @param m, the map of text to substitute
   * @returns a method that substitutes all strings in the input string ; the substitution may depend on the element processed.
   */
  def apply(m:Map[String,String]):(Element,String)=>String
}

/**
 * The default variable scheme:
 * - variables appear as $(name)
 * - the replacement is not recursive (i.e. a varibale name cannot refer to a variable)
 */
object varsSubstBuilder extends SubstBuilder {
  def apply(m:Map[String,String]):(Element,String)=>String = m match {
    case null => (_,s)=>s
    case map  => new utils.MapSubst(map).apply
  }
}

/**
 * The default variable scheme:
 * - variables appear as $(name)
 * - the replacement is not recursive (i.e. a varibale name cannot refer to a variable)
 */
object execSubstBuilder extends SubstBuilder {
  val s = new utils.ExecSubst[Element]
  def apply(m:Map[String,String]):(Element,String)=>String = s
}

/**
 * The default variable scheme:
 * - variables appear as $(name)
 * - the replacement is not recursive (i.e. a varibale name cannot refer to a variable)
 */
object fullSubstBuilder extends SubstBuilder {
  def apply(m:Map[String,String]):(Element,String)=>String = (ld,s) => execSubstBuilder.s(ld,varsSubstBuilder(m)(ld,s))
}

/**
 * The default variable scheme:
 * - variables appear as $(name)
 * - the replacement is not recursive (i.e. a varibale name cannot refer to a variable)
 */
object noSubstBuilder extends SubstBuilder {
  def apply(m:Map[String,String]):(Element,String)=>String = (_,s)=>s
}

