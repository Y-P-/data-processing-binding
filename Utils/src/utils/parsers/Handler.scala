package utils.parsers

trait Handler {
  def onInit(ps:State):Unit   //gives the parser state on initialization. This is how you can get the line count or current depth.
  //the standard client code sits here
  def push(idx:Int):Unit      //found array element
  def push(name:String):Unit  //found object field
  def pull(data:String):Unit  //read string value (array element or field value)
  def pull():Unit             //exiting object/array
  //managing errors, either from user code or from parser code
  def err(detail:String,cause:String):Nothing
  def handler:PartialFunction[Throwable,Unit]
}

//Peeking inside the parser's current state.
//This is handled once at the initialization of the parser.
trait State {
  def depth:Int      //current frame depth
  def line:Int       //current line number
  def str:String     //last string read
}