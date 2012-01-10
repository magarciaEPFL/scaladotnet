/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter
package session

/** A straight scalification of the jline interface which mixes
 *  in the sparse jline-independent one too.
 */
trait JLineHistory extends JHistory with History {
  def size: Int
  def isEmpty: Boolean
  def index: Int
  def clear(): Unit
  def get(index: Int): java.lang.CharSequence
  def add(line: java.lang.CharSequence): Unit
  def replace(item: java.lang.CharSequence): Unit

  def entries(index: Int): JListIterator/*[JEntry]*/
  def entries(): JListIterator/*[JEntry]*/
  def iterator: JIterator/*[JEntry]*/

  def current(): java.lang.CharSequence
  def previous(): Boolean
  def next(): Boolean
  def moveToFirst(): Boolean
  def moveToLast(): Boolean
  def moveTo(index: Int): Boolean
  def moveToEnd(): Unit
}

object JLineHistory {
  class JLineFileHistory extends SimpleHistory with FileBackedHistory {
    override def add(item: java.lang.CharSequence): Unit = {
      if (!isEmpty && last == item)
        repldbg("Ignoring duplicate entry '" + item + "'")
      else {
        super.add(item)
        addLineToFile(item)
      }
    }
    override def ToString = "History(size = " + size + ", index = " + index + ")"
  }

  def apply(): JLineHistory =
    try   { new JLineFileHistory }
    catch { case x: java.lang.Exception =>
      Console.println("Error creating file history: memory history only. " + x)
      util.Exceptional(x).show()
      new SimpleHistory()
    }
}