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

trait FmtBuilder[X,-E<:Def#Elt] {
  def apply[D1<:Def{ type Kind>:String }, E1<:D1#Elt](fd:FormatData):Formatter[X,E,E1]
}

object Formatters {
  
  protected class IntFormatter[D1<:Def{ type Kind>:String },E1<:D1#Elt](fmt:String) {
    def apply(x:Integer,e:Def#Elt,e1:E1) = e1.pull(String.format(fmt,x))    
  }

  object FmtJInt extends FmtBuilder[Integer,Def#Elt] {
    def apply[D1<:Def{ type Kind>:String }, E1<:D1#Elt](fd:FormatData) = new Formatter[Integer,Def#Elt,E1] {
      val fmt = if (fd.fmt==null || fd.fmt.length==0) "%d" else fd.fmt
      def apply(x:Integer,e:Def#Elt,e1:E1) = e1.pull(String.format(fmt,x))
    }
  }
  object FmtInt extends FmtBuilder[Int,Def#Elt] {
    def apply[D1<:Def{ type Kind>:String }, E1<:D1#Elt](fd:FormatData) = new Formatter[Int,Def#Elt,E1] {
      val fmt = if (fd.fmt==null || fd.fmt.length==0) "%d" else fd.fmt
      def apply(x:Int,e:Def#Elt,e1:E1) = e1.pull(String.format(fmt,new Integer(x)))
    }
  }
  object FmtJShort extends FmtBuilder[java.lang.Short,Def#Elt] {
    def apply[D1<:Def{ type Kind>:String }, E1<:D1#Elt](fd:FormatData) = new Formatter[java.lang.Short,Def#Elt,E1] {
      val fmt = if (fd.fmt==null || fd.fmt.length==0) "%d" else fd.fmt
      def apply(x:java.lang.Short,e:Def#Elt,e1:E1) = e1.pull(String.format(fmt,x))
    }
  }
  object FmtShort extends FmtBuilder[Short,Def#Elt] {
    def apply[D1<:Def{ type Kind>:String }, E1<:D1#Elt](fd:FormatData) = new Formatter[Short,Def#Elt,E1] {
      val fmt = if (fd.fmt==null || fd.fmt.length==0) "%d" else fd.fmt
      def apply(x:Short,e:Def#Elt,e1:E1) = e1.pull(String.format(fmt,new java.lang.Short(x)))
    }
  }

}