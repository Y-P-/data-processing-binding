package parser.px.analyzers.structured


/**
 * This is one possible mapping from the flat analyzer to a structured analyzer.
 * Here, we have imposed some choices, which seem reasonable.
 * - a field is either a container (contains other fields) or a leaf (contains data)
 * - a field can either have a name or be anonymous.
 * - all possibilities are opened, for the following classification:
 *   o name, leaf           : standard field              ; name = data
 *   o anonymous, data      : data in list                ; list = { data1 data2 }
 *   o name, container      : standard structure          ; name = { x=abc y={ 1 2 3 } z={ u=1 v=2}}
 *   o anonymous, container : structure found in "arrays" ; name = { {x=3 y=4} {x=2,y=5} }
 * This organization is logical, contains all possibilities offered, and leads itself to a number
 * of code simplifications.
 * It is important to note that the Named/Anonymous choice must come last in the trait stacking
 * list or bizarre result may sometimes happen when printing.
 * Also note that having the exact same type for DL,DSN,DA,DE leads to the interesting possibility
 * of specializing the empty field (when editing data), without having to change it's type.
 * 
 * Still, it is absolutely possible to define another kind of mapping.
 */
trait FieldDef {
  protected type Tk                  // The entering token class
  protected type R                   // The return type for the analyzer ; usually DSR
  protected type TKN                 // Class used for converting Tokens to names ; you can keep Tokens (T) that are passed as their lifetime is short 
  protected type Fld <: Field        // Base class for all fields which can be referenced from a Struct
  protected type DF  <: BaseField    // Base field
  protected type Dl  <: DataField    // List field element
  protected type DL  <: StructField  // List field
  protected type DSN <: StructField  // Struct field (normal)
  protected type DA  <: StructField  // Array field
  protected type DE  <: StructField  // Empty field
  protected type DSA <: ArrayField   // Struct field (anonymous)
  protected type DSR <: RootField    // Struct field (root)
  
  type BaseField   <: Fld with Leaf with Named
  type DataField   <: Fld with Leaf with Anonymous
  type StructField <: Fld with Container with Named
  type ArrayField  <: Fld with Container with Anonymous
  type RootField   <: Fld with Root with Container with Anonymous
}