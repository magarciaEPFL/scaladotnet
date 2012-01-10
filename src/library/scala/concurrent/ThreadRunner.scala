/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.lang.Thread

/** The `ThreadRunner` trait...
 *
 *  @author Philipp Haller
 */
class ThreadRunner extends FutureTaskRunner {

  type Task[T] = () => T
  type Future[T] = () => T

  implicit def functionAsTask[S](fun: () => S): Task[S] = fun
  implicit def futureAsFunction[S](x: Future[S]): () => S = x

  /* If expression computed successfully return it in `Right`,
   * otherwise return exception in `Left`.
   */
  private def tryCatch[A](body: => A): Either[java.lang.Exception, A] =
    try Right(body) catch {
      case ex: java.lang.Exception => Left(ex)
    }

  def execute[S](task: Task[S]) {
    val runnable = new java.lang.Runnable {
      def run() { tryCatch(task()) }
    }
    (new java.lang.Thread(runnable)).start()
  }

  def submit[S](task: Task[S]): Future[S] = {
    val result = new SyncVar[Either[java.lang.Exception, S]]
    val runnable = new java.lang.Runnable {
      def run() { result set tryCatch(task()) }
    }
    (new java.lang.Thread(runnable)).start()
    () => result.get.fold[S](throw _, identity _)
  }

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

  def shutdown() {
    // do nothing
  }

}
