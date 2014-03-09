package loader.core

import loader.core.definition.Processor

package object events {
  
  type EventHandler[-P<:Processor] = PartialFunction[(P#GenElt,Event),Unit]

}