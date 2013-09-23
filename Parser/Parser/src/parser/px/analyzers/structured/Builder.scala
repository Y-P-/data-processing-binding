package parser.px.analyzers.structured

/**
 * Utility class for building fields from scratch.
 * It also lets modify the current structure (remove fields, insert fields...)
 * 
 * Defines a simple DSL.
 * By default, does nothing.
 * It requires a specific implementation for every kind of Field implementation.
 * 
 * Available only through implicit/explicit conversions : creating a Builder from
 * scratch is discouraged : builders require intimate knowledge of the underlying
 * Field implementation.
 * 
 * All implementations don't have to support builders, nor do they have to support
 * all operations if they support it.
 */
protected class Builder(name:String) {
  /** Builds a Leaf or from a String and a String */
  def :=(value:String):Field with Leaf = throw new UnsupportedOperationException
  /** Builds a Container or from a String and a bunch of fields */
  def :=(values:Field*):Field with Container = throw new UnsupportedOperationException
}
