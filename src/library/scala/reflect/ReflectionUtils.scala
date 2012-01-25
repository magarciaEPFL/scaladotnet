/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.reflect

import java.lang.reflect.{ InvocationTargetException, UndeclaredThrowableException }

/** A few java-reflection oriented utility functions useful during reflection bootstrapping.
 */
object ReflectionUtils {
  // Unwraps some chained exceptions which arise during reflective calls.
  def unwrapThrowable(x: System.Exception): System.Exception = x match {
    case  _: InvocationTargetException |      // thrown by reflectively invoked method or constructor
          _: java.lang.ExceptionInInitializerError |    // thrown when running a static initializer (e.g. a scala module constructor)
          _: UndeclaredThrowableException |   // invocation on a proxy instance if its invocation handler's `invoke` throws an exception
          _: java.lang.ClassNotFoundException |         // no definition for a class instantiated by name
          _: java.lang.NoClassDefFoundError             // the definition existed when the executing class was compiled, but can no longer be found
            if java.lang.Throwable.instancehelper_getCause(x) != null =>
              unwrapThrowable(java.lang.Throwable.instancehelper_getCause(x))
    case _ => x
  }
  // Transforms an exception handler into one which will only receive the unwrapped
  // exceptions (for the values of wrap covered in unwrapThrowable.)
  def unwrapHandler[T](pf: PartialFunction[System.Exception, T]): PartialFunction[System.Exception, T] = {
    case ex if pf isDefinedAt unwrapThrowable(ex)   => pf(unwrapThrowable(ex))
  }

  def singletonInstance(className: String, cl: java.lang.ClassLoader = _root_.java.lang.Object.instancehelper_getClass(this).getClassLoader): AnyRef = {
    val name = if (className EndsWith "$") className else className + "$"
    val clazz = java.lang.Class.forName(name, true, cl) 
    val singleton = clazz getField "MODULE$" get null
    singleton
  }

  // Retrieves the MODULE$ field for the given class name.
  def singletonInstanceOpt(className: String, cl: java.lang.ClassLoader = _root_.java.lang.Object.instancehelper_getClass(this).getClassLoader): Option[AnyRef] =
    try Some(singletonInstance(className, cl))
    catch { case _: java.lang.ClassNotFoundException  => None }
}
