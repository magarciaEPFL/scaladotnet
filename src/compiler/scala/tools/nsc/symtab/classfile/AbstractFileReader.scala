/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package symtab
package classfile

import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import scala.tools.nsc.io.AbstractFile

/**
 * This class reads files byte per byte. Only used by ClassFileParser
 *
 * @author Philippe Altherr
 * @version 1.0, 23/03/2004
 */
class AbstractFileReader(val file: AbstractFile) {

  /** the buffer containing the file
   */
  val buf: Array[Byte] = file.toByteArray

  /** the current input pointer
   */
  var bp: Int = 0

  /** return byte at offset 'pos'
   */
  //@throws(classOf[IndexOutOfBoundsException])
  def byteAt(pos: Int): Byte = buf(pos)

  /** read a byte
   */
  //@throws(classOf[IndexOutOfBoundsException])
  def nextByte: Byte = {
    val b = buf(bp)
    bp += 1
    b
  }

  /** read some bytes
   */
  def nextBytes(len: Int): Array[Byte] = {
    bp += len
    buf.slice(bp - len, bp)
  }

  /** read a character
   */
  def nextChar: Char =
    (((nextByte & 0xff) << ((8) & 0x1f)) + (nextByte & 0xff)).toChar

  /** read an integer
   */
  def nextInt: Int =
    ((nextByte & 0xff) << ((24) & 0x1f)) + ((nextByte & 0xff) << ((16) & 0x1f)) +
    ((nextByte & 0xff) <<  ((8) & 0x1f)) +  (nextByte & 0xff)


  /** extract a character at position bp from buf
   */
  def getChar(mybp: Int): Char =
    (((buf(mybp) & 0xff) << ((8) & 0x1f)) + (buf(mybp+1) & 0xff)).toChar

  /** extract an integer at position bp from buf
   */
  def getInt(mybp: Int): Int =
    ((buf(mybp  ) & 0xff) << ((24) & 0x1f)) + ((buf(mybp+1) & 0xff) << ((16) & 0x1f)) +
    ((buf(mybp+2) & 0xff) << ((8) & 0x1f)) + (buf(mybp+3) & 0xff)

  /** extract a long integer at position bp from buf
   */
  def getLong(mybp: Int): Long =
    (getInt(mybp).toLong << ((32) & 0x3f)) + (getInt(mybp + 4) & 0xffffffffL)

  /** extract a float at position bp from buf
   */
  def getFloat(mybp: Int): Float = java.lang.Float.intBitsToFloat(getInt(mybp))

  /** extract a double at position bp from buf
   */
  def getDouble(mybp: Int): Double = java.lang.Double.longBitsToDouble(getLong(mybp))

  /** skip next 'n' bytes
   */
  def skip(n: Int) { bp += n }

}
