package loader.reflect

import loader.core.definition.Def

/** A formatter takes an input X, possibly associated with an Elt of some kind,
 *  and it manipulates an 'output' Elt of another kind.
 */
trait Formatter[X,-E<:Def#Elt,-E1<:Def#Elt] {
  def apply(x:X,e:E,e1:E1):Unit
}

trait FormatData {
  def fmt:String
}

class FmtInt[-E<:Def#Elt,+D1<:Def{ type Kind>:String }, E1<:D1#Elt](fd:FormatData) extends Formatter[Integer,E,E1] {
  def apply(x:Integer,e:E,e1:E1) = e1.pull(String.format(fd.fmt,x))
}