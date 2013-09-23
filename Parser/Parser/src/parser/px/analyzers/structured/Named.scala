package parser.px.analyzers.structured

/**
 * Indicates that a field has a name.
 * We intern the name because names are likely to be repeated ; it is also faster
 * to compare interned names, an operation that is likely to be done many times.
 */
protected trait Named extends Field {
  val name:String
  final abstract override def leftString = name + " = " + super.leftString
}
