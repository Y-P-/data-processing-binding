package object loader {
  import scala.language.existentials
  type Element = core.CtxCore.Def#Elt
  
  type Assoc[+k,+t]  = commons.Assoc[k,t]
  type Named         = commons.Named
  type NamedField[x] = commons.NamedField[x]
  
  type Context = loader.core.context.Context
  type InternalLoaderException = loader.core.exceptions.InternalLoaderException
  type UserException = loader.core.exceptions.UserException
}