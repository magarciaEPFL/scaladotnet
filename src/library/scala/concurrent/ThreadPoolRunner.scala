/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import java.util.concurrent.{ExecutorService, Callable, TimeUnit}

/** The `ThreadPoolRunner` trait uses a `java.util.concurrent.ExecutorService`
 *  to run submitted tasks.
 *
 *  @author Philipp Haller
 */
trait ThreadPoolRunner extends FutureTaskRunner {

  type Task[T] = Callable/*[T] MANUALLY */ with java.lang.Runnable
  type Future[T] = java.util.concurrent.Future/*[T] MANUALLY */

  private class RunCallable[S](fun: () => S) extends java.lang.Runnable with Callable/*[S]*/ {
    def run() = fun()
    def call() = fun().asInstanceOf[System.Object] /* MANUALLY */
  }

  implicit def functionAsTask[S](fun: () => S): Task[S] =
    new RunCallable(fun)

  implicit def futureAsFunction[S](x: Future[S] /* MANUALLY */): () => S =
    () => (x.get()).asInstanceOf[S]

  protected def executor: ExecutorService

  def submit[S](task: Task[S]): Future[S] /* MANUALLY */ = {
    executor.submit/*[S] MANUALLY */(task.asInstanceOf[Callable])
  }

  def execute[S](task: Task[S]) {
    executor execute task
  }

  def managedBlock(blocker: ManagedBlocker) {
    blocker.block()
  }

}
