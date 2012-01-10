/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import java.lang.Character

final class RichChar(val self: Char) extends IntegralProxy[Char] {
  def asDigit: Int                      = java.lang.Character.digit(self, Character.MAX_RADIX)

  def isControl: Boolean                = java.lang.Character.isISOControl(self)
  def isDigit: Boolean                  = java.lang.Character.isDigit(self)
  def isLetter: Boolean                 = java.lang.Character.isLetter(self)
  def isLetterOrDigit: Boolean          = java.lang.Character.isLetterOrDigit(self)
  def isWhitespace: Boolean             = java.lang.Character.isWhitespace(self)
  def isSpaceChar: Boolean              = java.lang.Character.isSpaceChar(self)
  def isHighSurrogate: Boolean          = java.lang.Character.isHighSurrogate(self)
  def isLowSurrogate: Boolean           = java.lang.Character.isLowSurrogate(self)
  def isSurrogate: Boolean              = isHighSurrogate || isLowSurrogate
  def isUnicodeIdentifierStart: Boolean = java.lang.Character.isUnicodeIdentifierStart(self)
  def isUnicodeIdentifierPart: Boolean  = java.lang.Character.isUnicodeIdentifierPart(self)
  def isIdentifierIgnorable: Boolean    = java.lang.Character.isIdentifierIgnorable(self)
  def isMirrored: Boolean               = java.lang.Character.isMirrored(self)

  def isLower: Boolean                  = java.lang.Character.isLowerCase(self)
  def isUpper: Boolean                  = java.lang.Character.isUpperCase(self)
  def isTitleCase: Boolean              = java.lang.Character.isTitleCase(self)

  def toLower: Char                     = java.lang.Character.toLowerCase(self)
  def toUpper: Char                     = java.lang.Character.toUpperCase(self)
  def toTitleCase: Char                 = java.lang.Character.toTitleCase(self)

  def getType: Int                      = java.lang.Character.getType(self)
  def getNumericValue: Int              = java.lang.Character.getNumericValue(self)
  def getDirectionality: Byte           = java.lang.Character.getDirectionality(self)
  def reverseBytes: Char                = java.lang.Character.reverseBytes(self)

  // Java 5 Character methods not added:
  //
  // public static boolean isDefined(char ch)
  // public static boolean isJavaIdentifierStart(char ch)
  // public static boolean isJavaIdentifierPart(char ch)
}
