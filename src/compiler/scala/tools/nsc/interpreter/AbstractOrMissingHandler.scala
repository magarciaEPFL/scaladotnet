/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

class AbstractOrMissingHandler[T](onError: String => Unit, value: T) extends scala.runtime.AbstractPartialFunction[System.Exception, T] {
  def _isDefinedAt(t: System.Exception) = t match {
    case _: AbstractMethodError     => true
    case _: java.lang.NoSuchMethodError       => true
    case _: MissingRequirementError => true
    case _: java.lang.NoClassDefFoundError    => true
    case _                          => false
  }
  def apply(t: System.Exception) = t match {
    case x @ (_: AbstractMethodError | _: java.lang.NoSuchMethodError | _: java.lang.NoClassDefFoundError) =>
      onError("""
        |Failed to initialize compiler: %s.
        |This is most often remedied by a full clean and recompile.
        |Otherwise, your classpath may continue bytecode compiled by
        |different and incompatible versions of scala.
        |""".stripMargin.format(_root_.java.lang.Object.instancehelper_getClass(x).getName split '.' last)
      )
      java.lang.Throwable.instancehelper_printStackTrace(x)
      value
    case x: MissingRequirementError =>
      onError("""
        |Failed to initialize compiler: %s not found.
        |** Note that as of 2.8 scala does not assume use of the java classpath.
        |** For the old behavior pass -usejavacp to scala, or if using a Settings
        |** object programatically, settings.usejavacp.value = true.""".stripMargin.format(x.req)
      )
      value
  }
}

object AbstractOrMissingHandler {
  def apply[T]() = new AbstractOrMissingHandler[T](Console println _, null.asInstanceOf[T])
}
