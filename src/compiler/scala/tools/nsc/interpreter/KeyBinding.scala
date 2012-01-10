/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

case class KeyBinding(name: String, code: Int, aliases: List[String], description: String) {
  def nameString = if (aliases.nonEmpty) aliases mkString ", " else name
  override def ToString = "%3d %s: %s".format(code, nameString, description)
}

object KeyBinding {
  def parse(bindings: String): List[KeyBinding] = {
    def loop(xs: List[String]): List[KeyBinding] = {
      val (comment, lines) = xs span (_ StartsWith "#")
      val description = comment map (c => c drop 1 Trim) mkString " "
      val (aliases, desc) = description span (_ != ':') match {
        case (x, y) => (
          x split ',' map (_.Trim) toList,
          if (y == "") "" else y.tail.Trim
        )
      }
      lines match {
        case Nil      => Nil
        case hd :: tl =>
          val kb = (hd IndexOf '=') match {
            case -1   => KeyBinding(hd, -1, aliases, desc)
            case idx  => KeyBinding(hd drop idx + 1, hd take idx toInt, aliases, desc)
          }
          kb :: loop(tl)
      }
    }
    // This is verrrrrrrry specific to the current contents
    // of the keybindings.properties in jline.
    loop(_root_.java.lang.String.instancehelper_split(bindings, "\\n") map (_.Trim) dropWhile (_ != "") filterNot (_ == "") toList) sortBy (_.code)
  }
}
