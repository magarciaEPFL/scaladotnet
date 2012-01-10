/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package immutable

import generic._
import mutable.Builder
import scala.util.matching.Regex
import scala.math.ScalaNumber

/** A companion object for the `StringLike` containing some constants.
 *  @since 2.8
 */
object StringLike {

  // just statics for companion class.
  private final val LF: Char = 0x0A
  private final val FF: Char = 0x0C
  private final val CR: Char = 0x0D
  private final val SU: Char = 0x1A
}

import StringLike._

/** A trait describing stringlike collections.
 *
 *  @tparam Repr   The type of the actual collection inheriting `StringLike`.
 *
 *  @since 2.8
 *  @define Coll String
 *  @define coll string
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
trait StringLike[+Repr] extends collection.IndexedSeqOptimized[Char, Repr] with Ordered[String] {
self =>

  /** Creates a string builder buffer as builder for this class */
  protected[this] def newBuilder: Builder[Char, Repr]

  /** Return element at index `n`
   *  @throws   IndexOutOfBoundsException if the index is not valid
   */
  def apply(n: Int): Char = ToString Chars n

  def length: Int = ToString.Length

  override def mkString = ToString

  override def slice(from: Int, until: Int): Repr = {
    val start = from max 0
    val end   = until min length

    if (start >= end) newBuilder.result
    else newBuilder ++= _root_.java.lang.String.instancehelper_substring(ToString, start, end) result
  }

  /** Return the current string concatenated `n` times.
   */
  def * (n: Int): String = {
    val buf = new StringBuilder
    for (i <- 0 until n) buf append ToString
    buf.ToString
  }

  override def compare(other: String) = _root_.java.lang.String.instancehelper_compareTo(ToString, other)

  private def isLineBreak(c: Char) = c == LF || c == FF

  /**
   *  Strip trailing line end character from this string if it has one.
   *
   *  A line end character is one of
   *  - `LF` - line feed   (`0x0A` hex)
   *  - `FF` - form feed   (`0x0C` hex)
   *
   *  If a line feed character `LF` is preceded by a carriage return `CR`
   *  (`0x0D` hex), the `CR` character is also stripped (Windows convention).
   */
  def stripLineEnd: String = {
    val len = ToString.Length
    if (len == 0) ToString
    else {
      val last = apply(len - 1)
      if (isLineBreak(last))
        _root_.java.lang.String.instancehelper_substring(ToString, 0, if (last == LF && len >= 2 && apply(len - 2) == CR) len - 2 else len - 1)
      else
        ToString
    }
  }

  /** Return all lines in this string in an iterator, including trailing
   *  line end characters.
   *
   *  The number of strings returned is one greater than the number of line
   *  end characters in this string. For an empty string, a single empty
   *  line is returned. A line end character is one of
   *  - `LF` - line feed   (`0x0A` hex)
   *  - `FF` - form feed   (`0x0C` hex)
   */
  def linesWithSeparators: Iterator[String] = new AbstractIterator[String] {
    val str = self.ToString
    private val len = str.Length
    private var index = 0
    def hasNext: Boolean = index < len
    def next(): String = {
      if (index >= len) throw new NoSuchElementException("next on empty iterator")
      val start = index
      while (index < len && !isLineBreak(apply(index))) index += 1
      index += 1
      _root_.java.lang.String.instancehelper_substring(str, start, index min len)
    }
  }

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e. apply `.stripLineEnd` to all lines
   *  returned by `linesWithSeparators`.
   */
  def lines: Iterator[String] =
    linesWithSeparators map (line => new WrappedString(line).stripLineEnd)

  /** Return all lines in this string in an iterator, excluding trailing line
   *  end characters, i.e. apply `.stripLineEnd` to all lines
   *  returned by `linesWithSeparators`.
   */
  def linesIterator: Iterator[String] =
    linesWithSeparators map (line => new WrappedString(line).stripLineEnd)

  /** Returns this string with first character converted to upper case */
  def capitalize: String =
    if (ToString == null) null
    else if (ToString.Length == 0) ""
    else {
      val chars = ToString.ToCharArray
      chars(0) = chars(0).toUpper
      java.lang.String.newhelper(chars) 
    }

  /** Returns this string with the given `prefix` stripped. */
  def stripPrefix(prefix: String) =
    if (ToString.StartsWith(prefix)) ToString.Substring(prefix.Length)
    else ToString

  /** Returns this string with the given `suffix` stripped. If this string does not
    * end with `suffix`, it is returned unchanged. */
  def stripSuffix(suffix: String) =
    if (ToString.EndsWith(suffix)) _root_.java.lang.String.instancehelper_substring(ToString, 0, ToString.Length - suffix.Length)
    else ToString

  /** Replace all literal occurrences of `literal` with the string `replacement`.
   *  This is equivalent to [[java.lang.String#replaceAll]] except that both arguments
   *  are appropriately quoted to avoid being interpreted as metacharacters.
   *
   *  @param    literal     the string which should be replaced everywhere it occurs
   *  @param    replacement the replacement string
   *  @return               the resulting string
   */
  def replaceAllLiterally(literal: String, replacement: String): String = {
    val arg1 = java.util.regex.Pattern.quote(literal)
    val arg2 = java.util.regex.Matcher.quoteReplacement(replacement)

    _root_.java.lang.String.instancehelper_replaceAll(ToString, arg1, arg2)
  }

  /** For every line in this string:
   *
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `marginChar` from the line.
   */
  def stripMargin(marginChar: Char): String = {
    val buf = new StringBuilder
    for (line <- linesWithSeparators) {
      val len = line.Length
      var index = 0
      while (index < len && line.Chars(index) <= ' ') index += 1
      buf append
        (if (index < len && line.Chars(index) == marginChar) line.Substring(index + 1) else line)
    }
    buf.ToString
  }

  /** For every line in this string:
   *
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `|` from the line.
   */
  def stripMargin: String = stripMargin('|')

  private def escape(ch: Char): String = "\\Q" + ch + "\\E"

  //@throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separator: Char): Array[String] = _root_.java.lang.String.instancehelper_split(ToString, escape(separator))

  //@throws(classOf[java.util.regex.PatternSyntaxException])
  def split(separators: Array[Char]): Array[String] = {
    val re = separators.foldLeft("[")(_+escape(_)) + "]"
    _root_.java.lang.String.instancehelper_split(ToString, re)
  }

  /** You can follow a string with `.r`, turning it into a `Regex`. E.g.
   *
   *  """A\w*""".r   is the regular expression for identifiers starting with `A`.
   */
  def r: Regex = new Regex(ToString)

  def toBoolean: Boolean = parseBoolean(ToString)
  def toByte: Byte       = java.lang.Byte.parseByte(ToString)
  def toShort: Short     = java.lang.Short.parseShort(ToString)
  def toInt: Int         = java.lang.Integer.parseInt(ToString)
  def toLong: Long       = java.lang.Long.parseLong(ToString)
  def toFloat: Float     = java.lang.Float.parseFloat(ToString)
  def toDouble: Double   = java.lang.Double.parseDouble(ToString)

  private def parseBoolean(s: String): Boolean =
    if (s != null) s.ToLower match {
      case "true" => true
      case "false" => false
      case _ => throw new java.lang.IllegalArgumentException("For input string: \""+s+"\"")
    }
    else
      throw new java.lang.IllegalArgumentException("For input string: \"null\"")

  override def toArray[B >: Char : ClassManifest]: Array[B] =
    ToString.ToCharArray.asInstanceOf[Array[B]]

  private def unwrapArg(arg: Any): AnyRef = arg match {
    case x: ScalaNumber => x.underlying
    case x              => x.asInstanceOf[AnyRef]
  }

  /** Uses the underlying string as a pattern (in a fashion similar to
   *  printf in C), and uses the supplied arguments to fill in the
   *  holes.
   *
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    `java.util.Formatter`</a>, with the addition that
   *    classes deriving from `ScalaNumber` (such as [[scala.BigInt]] and
   *    [[scala.BigDecimal]]) are unwrapped to pass a type which `Formatter`
   *    understands.
   *
   *  @param args the arguments used to instantiating the pattern.
   *  @throws `java.lang.IllegalArgumentException`
   */
  def format(args : Any*): String =  /* MANUALLY */
    try {
      val unwr = (args map unwrapArg) map { arg => 
        // the IKVM implementation expects e.g. j.l.Float not a System.Float wrapped in a System.Object
        if     (arg.isInstanceOf[Float])  new java.lang.Float  (arg.asInstanceOf[Float])
        else if(arg.isInstanceOf[Double]) new java.lang.Double (arg.asInstanceOf[Double])
        else if(arg.isInstanceOf[Int])    new java.lang.Integer(arg.asInstanceOf[Int])
        else arg
      }
      java.lang.String.format(ToString, unwr.toArray)
    } catch {
      case ex: java.util.IllegalFormatException => args.ToString()
    }

  /** Like `format(args*)` but takes an initial `Locale` parameter
   *  which influences formatting as in `java.lang.String`'s format.
   *
   *    The interpretation of the formatting patterns is described in
   *    <a href="" target="contentFrame" class="java/util/Formatter">
   *    `java.util.Formatter`</a>, with the addition that
   *    classes deriving from `ScalaNumber` (such as `scala.BigInt` and
   *    `scala.BigDecimal`) are unwrapped to pass a type which `Formatter`
   *    understands.
   *
   *  @param locale an instance of `java.util.Locale`
   *  @param args the arguments used to instantiating the pattern.
   *  @throws `java.lang.IllegalArgumentException`
   */
  def formatLocal(l: java.util.Locale, args: Any*): String =
    java.lang.String.format(l, ToString, (args map unwrapArg).toArray ) /* MANUALLY */
}
