package parser.px.analyzers.structured

/**
 * Indicates that a field has no name.
 * Usually used for anonymous structures in arrays.
 * Can also be used for elements in a list.
 */
protected trait Anonymous extends Field {
  final def name = nameAsStr
  final protected def nameAsStr:String = ""
}
