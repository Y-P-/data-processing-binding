package parser.px.analyzers.structured

/**
 * Leaf fields which do not contain other fields.
 * This applies to data in lists like list = { val1 val2 } and base fields like field = data
 */
protected trait Leaf extends Field {
  final def ->(name:String):Nothing      = throw new IllegalStateException("trying to dereference leaf field")
  final def ->(b:Binder):Nothing         = throw new IllegalStateException("trying to dereference leaf field")
  final override def :=(newValues:Field*):Nothing          = throw new UnsupportedOperationException("trying to fill up a non container field")
  final override def insertAt(old:Field,fld:Field):Unit    = throw new UnsupportedOperationException("trying to insert a field in a non container field")
  final override def insertAfter(old:Field,fld:Field):Unit = throw new UnsupportedOperationException("trying to insert a field in a non container field")
  final override def replace(old:Field,fld:Field):Unit     = throw new UnsupportedOperationException("trying to replace a field in a non container field")
  final override def delete(old:Field):Unit                = throw new UnsupportedOperationException("trying to remove a field from a non container field")
  def select(name:String):Nothing        = throw new IllegalStateException("trying to dereference leaf field")
  def value = valueAsStr
  protected def valueAsStr:String
  def rightString:String = value   
  def leftString = ""
}
