/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package io

import java.io.{ InputStream }
import java.util.jar.JarEntry

/** A common interface for File-based things and Stream-based things.
 *  (In particular, io.File and JarEntry.)
 */
class Fileish(val path: Path, val input: () => InputStream) extends Streamable.Chars {
  def inputStream() = input()

  def parent       = path.parent
  def name         = path.name
  def isSourceFile = path.hasExtension("java", "scala")

  private lazy val pkgLines = lines() collect { case x if x StartsWith "package " => x stripPrefix "package" Trim }
  lazy val pkgFromPath      = _root_.java.lang.String.instancehelper_replaceAll(parent.path, """[/\\]""", ".")
  lazy val pkgFromSource    = pkgLines map (_ stripSuffix ";") mkString "."

  override def ToString = path.path
}

object Fileish {
  def apply(f: File): Fileish = new Fileish(f, () => f.inputStream())
  def apply(f: JarEntry, in: () => InputStream): Fileish  = new Fileish(Path(f.getName), in)
  def apply(path: String, in: () => InputStream): Fileish = new Fileish(Path(path), in)
}
