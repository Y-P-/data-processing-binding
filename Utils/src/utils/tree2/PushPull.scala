package utils.tree2

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

abstract class PushPull[-K,+V] {
  def push(key:K):Unit
  protected[this] def pull(value:V):Unit
  def pull():Unit
}
