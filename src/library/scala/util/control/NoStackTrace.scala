/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.control

/** A trait for exceptions which, for efficiency reasons, do not
 *  fill in the stack trace.  Stack trace suppression can be disabled
 *  on a global basis via a system property wrapper in
 *  [[scala.sys.SystemProperties]].
 *
 *  @author   Paul Phillips
 *  @since    2.8
 */
trait NoStackTrace extends java.lang.Throwable {
  override def fillInStackTrace(): System.Exception =
    if (NoStackTrace.noSuppression)  java.lang.Throwable.instancehelper_fillInStackTrace(this)
    else this
}

object NoStackTrace {
  final val noSuppression = sys.SystemProperties.noTraceSupression.value
}
