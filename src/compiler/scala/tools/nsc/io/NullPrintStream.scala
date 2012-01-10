/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package io

import java.io.{ PrintStream, ByteArrayOutputStream }

/** A sink for when you want to discard all output.
 */
class NullPrintStream extends PrintStream(new ByteArrayOutputStream()) {
  override def Dispose { this.close }
}

object NullPrintStream extends NullPrintStream {
  def setOut() = Console setOut this
  def setErr() = Console setErr this
  def setOutAndErr() = { setOut() ; setErr() }
  def sinkingOutAndErr[T](body: => T): T =
    Console.withOut(this) {
      Console.withErr(this) {
        body
      }
    }

  def sinkingSystemOutAndErr[T](body: => T): T = {
    val savedOut = java.lang.System.out
    val savedErr = java.lang.System.err
    java.lang.System setOut NullPrintStream
    java.lang.System setErr NullPrintStream
    try body
    finally {
      java.lang.System setOut savedOut
      java.lang.System setErr savedErr
    }
  }
  override def Dispose { this.close }
}
