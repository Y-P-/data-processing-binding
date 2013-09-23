package loader.features

import loader.Element
import loader.exceptions._

/**
 * Handler for errors.
 * The standard way is to handle all exceptions as to analyze as far as possible,
 * but leave InternalLoaderException as the only way out.
 * This exception may be used by the user's code to handle cases that require exiting.
 * Any other error would be handled.
 */
trait ErrorHandler {
  @throws(classOf[UserException])
  @throws(classOf[InternalLoaderException])
  def onPush(ld:Element):PartialFunction[Throwable,Element]
  @throws(classOf[UserException])
  @throws(classOf[InternalLoaderException])
  def onPull(ld:Element):PartialFunction[Throwable,Unit]
}

object handleAll extends ErrorHandler {
  @throws(classOf[UserException])
  @throws(classOf[InternalLoaderException])
  def onPush(ld:Element):PartialFunction[Throwable,Element] = {
    case e:InternalLoaderException => throw e
    case e:UserException           => if (e.lvl<=1) throw e else null
    case _                         => null
  }
  @throws(classOf[UserException])
  @throws(classOf[InternalLoaderException])
  def onPull(ld:Element):PartialFunction[Throwable,Unit]   = {
    case e:InternalLoaderException => throw e
    case e:UserException           => if (e.lvl<=1) throw e
    case _ => ()
  }
}
object throwAll extends ErrorHandler {
  def onPush(ld:Element):PartialFunction[Throwable,Element] = { case e => throw e }
  def onPull(ld:Element):PartialFunction[Throwable,Unit]   = { case e => throw e }
}
