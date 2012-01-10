package scala.tools.nsc.scratchpad

import java.io.{PrintStream, OutputStreamWriter, Writer}

import scala.runtime.ScalaRunTime.stringOf
import java.lang.reflect.InvocationTargetException
import scala.reflect.ReflectionUtils._

object Executor {

  println("exec started")

  private var currentWriter: CommentWriter = null

  /** Execute module with given name, redirecting all output to given
   *  source inserter. Catch all exceptions and print stacktrace of underlying causes.
   */
  def execute(name: String, si: SourceInserter, classLoader: java.lang.ClassLoader = _root_.java.lang.Object.instancehelper_getClass(this).getClassLoader) {
    val oldSysOut = java.lang.System.out
    val oldSysErr = java.lang.System.err
    val oldConsOut = Console.out
    val oldConsErr = Console.err
    val oldCwr = currentWriter
    currentWriter = new CommentWriter(si)
    val newOut = new PrintStream(new CommentOutputStream(currentWriter))
    java.lang.System.setOut(newOut)
    java.lang.System.setErr(newOut)
    Console.setOut(newOut)
    Console.setErr(newOut)
    try {
      singletonInstance(name, classLoader)
    } catch {
      case ex: System.Exception =>
        unwrapThrowable(ex) match {
          case _: StopException => ;
          case cause => java.lang.Throwable.instancehelper_printStackTrace(cause)
        }
    } finally {
      currentWriter.close()
      java.lang.System.setOut(oldSysOut)
      java.lang.System.setErr(oldSysErr)
      Console.setOut(oldConsOut)
      Console.setErr(oldConsErr)
      currentWriter = oldCwr
    }
  }

  def $skip(n: Int) = currentWriter.skip(n)

  def $stop() = throw new StopException

  def $show(x: Any): String = stringOf(x, scala.Int.MaxValue)
}

class StopException extends java.lang.Exception
