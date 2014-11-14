package utils.parsers

import utils.CharReader

/** Describes the methods expected by a parser
 */
trait Handler {
  /** informs that an array elt of index idx is forthwoming */
  def push(idx:Int):Unit
  /** informs that an elt of the given name is forthwoming */
  def push(name:String):Unit
  /** informs about the data read for an array/named element */
  def pull(data:String):Unit
  /** informs that a layer (array/object) is about to end */
  def pull():Unit
  
  /** this is invoked when the parser meets an internal error.
   *  you can throw any exception but MatchError or parser exceptions.
   */
  def err(detail:String,cause:String):Nothing
  /** this is invoked when your code (push/pull) throws an exception.
   *  this may or may not halt the parser. You can call err if you want.
   *  anyway as a good programming practise, your user code should not
   *  throw exceptions, so this handler is only there in case.
   */
  def handler:PartialFunction[Throwable,Unit]
  /** this is provided by a given parser in order to fast skip data.
   *  your code cannot rely on this and must be ready to handle the
   *  case where the parser doesn't support it ; in that case it
   *  returns false.
   */
  def abort(n:Int):Boolean
  
  /** this is the actual parser type */
  protected type Processor <: CharProcessor
  /** this creates a parser */
  def newProc:Processor
}

/** Peeks inside the parser's current state.
 *  This is handled at the initialization of the parser.
 *  You don't have to recover it if you don't care.
 *  Depending on the parser, the information is not always accurate,
 *  (i.e. it doesn't always point to the exact place of the token
 *  currently seen by the user code) but usually close to and always
 *  after.
 */
trait State {
  /** current frame depth */
  def depth:Int
  /** current line number */
  def line:Int
  /** last string read */
  def str:String
}

/** Defines the standard way to start the Parser.
 */
trait CharProcessor {
  /** runs the parser on the provided character flow */
  def apply(d:CharReader):Unit
  /** the current parser state */
  def state:State
}
