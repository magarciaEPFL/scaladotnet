/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

class MissingRequirementError private (msg: String) extends FatalError(msg) {
  import MissingRequirementError.suffix
  def req: String = if (msg EndsWith suffix) msg dropRight suffix.Length else msg
}

object MissingRequirementError {
  private val suffix = " not found."
  def signal(msg: String): Nothing = throw new MissingRequirementError(msg)
  def notFound(req: String): Nothing = signal(req + suffix)
  def unapply(x: System.Exception): Option[String] = x match {
    case x: MissingRequirementError => Some(x.req)
    case _ => None
  }
}


