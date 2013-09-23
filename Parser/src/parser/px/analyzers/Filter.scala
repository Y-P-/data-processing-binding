package parser.px.analyzers

import parser.px.Analyzer

trait Filter {
  protected type Tk
  def ignore(name:Tk):Boolean  
}

trait NoFilter extends Filter {
  protected type Tk
  def ignore(name:Tk):Boolean  = false  
}
