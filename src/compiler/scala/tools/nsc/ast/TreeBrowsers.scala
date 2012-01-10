/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

/**
 * Just a stub in Scala.Net, avoids dependency on window and 2D drawing stuff.
 */
abstract class TreeBrowsers {
  val global: Global
  import global._

  def create(): SwingBrowser = new SwingBrowser();

  class SwingBrowser {

    def browse(t: Tree): Tree = {
      t
    }

    def browse(pName: String, units: Iterator[CompilationUnit]): Unit =
      browse(pName, units.toList)

    /** print the whole program */
    def browse(pName: String, units: List[CompilationUnit]): Unit = { }

  }


}
