package utils.hparser

trait Handler {
  //the standard client code sits here
  def push(ps:State,name:String):Unit
  def pull(ps:State,data:String):Unit
  def pull(ps:State):Unit
  //managing errors, either from user code or from parser code
  def err(ps:State,detail:String,cause:String):Nothing
  def handler:PartialFunction[Throwable,Unit]
}

//Used to peek inside the parser current state.
trait Spy {
  var line:Int
  var depth:Int
}

//You don't have to peek unless for special needs.
//The parser already handles its current state.
trait State {
  def depth:Int      //current frame depth
  def line:Int       //current line number
  def str:String     //last string read
}
