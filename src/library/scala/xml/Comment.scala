/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml

/** The class `Comment` implements an XML node for comments.
 *
 * @author Burak Emir
 * @param text the text contained in this node, may not contain "--"
 */
case class Comment(commentText: String) extends SpecialNode {

  def label = "#REM"
  override def text = ""
  final override def doCollectNamespaces = false
  final override def doTransform         = false

  if (_root_.java.lang.String.instancehelper_contains(commentText, "--"))
    throw new java.lang.IllegalArgumentException("text contains \"--\"")

  /** Appends &quot;<!-- text -->&quot; to this string buffer.
   */
  override def buildString(sb: StringBuilder) =
    sb append "<!--" append commentText append "-->"
}
