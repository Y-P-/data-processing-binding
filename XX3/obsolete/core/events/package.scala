package loader.core

import loader.core.definition.Def

package object events {
  
  type EventHandler[-E<:Def#Elt] = PartialFunction[(E,Event),Unit]

}