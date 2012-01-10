/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * 
 */


package scala.tools.nsc
package backend.jvm

// stub only in Scala.Net
abstract class GenJVM extends SubComponent  {

  val phaseName = "jvm"

  override def newPhase(p: Phase): Phase = null

}
