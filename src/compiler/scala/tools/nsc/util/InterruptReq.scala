package scala.tools.nsc
package util

/** A class of work items to be used in interrupt requests.
 */
abstract class InterruptReq {
  /** The result type of the operation
   */
  type R

  /** The operation to be performed */
  protected val todo: () => R

  /** The result provided */
  private var result: Option[Either[R, System.Exception]] = None

  /** To be called from interrupted server to execute demanded task */
  def execute(): Unit = synchronized {
    try {
      result = Some(Left(todo()))
    } catch {
      case t => result = Some(Right(t))
    }
    _root_.java.lang.Object.instancehelper_notify(this)
  }

  /** To be called from interrupting client to get result for interrupt */
  def getResult(): R = synchronized {
    while (result.isEmpty) {
      try {
        _root_.java.lang.Object.instancehelper_wait(this)
      } catch { case _ : java.lang.InterruptedException => () }
    }

    result.get match {
      case Left(res) => res
      case Right(t) => throw new FailedInterrupt(t)
    }
  }
}

class FailedInterrupt(cause: System.Exception) extends java.lang.Exception("Compiler exception during call to 'ask'", cause)
