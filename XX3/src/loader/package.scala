package object loader {
  type Element = core.CtxCore.Processor#Element
  
  type Assoc[+k,+t]  = commons.Assoc[k,t]
  type Named         = commons.Named
  
  type Context = loader.core.context.Context
  type InternalLoaderException = loader.core.exceptions.InternalLoaderException
  type UserException = loader.core.exceptions.UserException
}