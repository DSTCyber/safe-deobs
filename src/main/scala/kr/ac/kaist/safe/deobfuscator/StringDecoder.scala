/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * Copyright (c) 2017, DST Group.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.deobfuscator

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.util.NodeUtil

/**
 * Decodes string literals to make them more readable. This includes:
 *
 *  - Converting hexadecimal escape sequences to ASCII values when possible.
 *    E.g. `\x48\x65\x6c\x6c\x6f` becomes "Hello"
 *  - URI-encoded strings are converted to readable strings. E.g. the string
 *    `%48%65%6C%6C%6F` becomes "Hello"
 */
class StringDecoder(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = StringRewriteWalker.walk(program)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  private object StringRewriteWalker extends ASTWalker {
    /**
     * Combine a sequence of chars into a single char.
     */
    private def combineChars(xs: Seq[Char]): Int =
      xs.foldLeft(0)(_ * 16 + Character.digit(_, 16)).toChar

    /**
     * Check if the given char is printable.
     */
    private def isPrintable(i: Int): Boolean =
      (i >= 0x07 && i <= 0x0d) || (i >= 0x20 && i <= 0x7e)

    /**
     * Escape special characters.
     */
    private def escapeChar(c: Char, quote: Char): Seq[Char] =
      if (c == '\\' || c == quote) Seq('\\', c)
      else NodeUtil.pp(c.toString)

    /**
     * Hex-encoded characters are converted to ASCII values when possible.
     * Quote (" and ') characters within the character sequence must be escaped
     * so that the string can be re-terminated.
     */
    private def unescapeHex(seq: Seq[Char], quote: Char): Seq[Char] = seq match {
      case Seq('\\', x1, x2, x3, xs @ _*) if Character.toLowerCase(x1) == 'x' &&
        Character.digit(x2, 16) != -1 &&
        Character.digit(x3, 16) != -1 =>
        val i = combineChars(List(x2, x3))
        val pre = if (isPrintable(i)) escapeChar(i.toChar, quote) else Seq('\\', x1, x2, x3)
        pre ++ unescapeHex(xs, quote)
      case Seq(x, xs @ _*) => x +: unescapeHex(xs, quote)
      case Seq() => ""
    }

    private def unescapeHex(str: String, quote: Char): String =
      unescapeHex(str.toSeq, quote).mkString

    /**
     * Unicode-encoded characters are converted to ASCII values where possible.
     * Quote (" and ') characters within the character sequence must be escaped
     * so that the string can be re-terminated.
     */
    private def unescapeUnicode(seq: Seq[Char], quote: Char): Seq[Char] = seq match {
      case Seq('\\', x1, x2, x3, x4, x5, xs @ _*) if Character.toLowerCase(x1) == 'u' &&
        Character.digit(x2, 16) != -1 &&
        Character.digit(x3, 16) != -1 &&
        Character.digit(x4, 16) != -1 &&
        Character.digit(x5, 16) != -1 =>
        val i = combineChars(List(x2, x3, x4, x5))
        val pre = if (isPrintable(i)) escapeChar(i.toChar, quote) else Seq('\\', x1, x2, x3, x4, x5)
        pre ++ unescapeUnicode(xs, quote)
      case Seq(x, xs @ _*) => x +: unescapeUnicode(xs, quote)
      case Seq() => ""
    }

    private def unescapeUnicode(str: String, quote: Char): String =
      unescapeUnicode(str.toSeq, quote).mkString

    /**
     * URI-encoded characters are converted to ASCII values when possible.
     * Quote (" and '') characters within the character sequence must be escaped
     * so that the string can be re-terminated.
     */
    private def unescapeUri(seq: Seq[Char], quote: Char): Seq[Char] = seq match {
      case Seq('%', x1, x2, xs @ _*) if Character.digit(x1, 16) != -1 &&
        Character.digit(x2, 16) != -1 =>
        val i = combineChars(List(x1, x2))
        val pre = if (isPrintable(i)) escapeChar(i.toChar, quote) else Seq('%', x1, x2)
        pre ++ unescapeUri(xs, quote)
      case Seq(x, xs @ _*) => x +: unescapeUri(xs, quote)
      case Seq() => ""
    }

    private def unescapeUri(str: String, quote: Char): String =
      unescapeUri(str.toSeq, quote).mkString

    override def walk(node: LHS): LHS = node match {
      // Compose unescape functions together so that we can unescape everything
      // at once
      case StringLiteral(info, quote, str, false) if quote.length == 1 =>
        val quoteChar = quote.charAt(0)
        val unescapeHexQuote = unescapeHex(_: String, quoteChar)
        val unescapeUnicodeQuote = unescapeUnicode(_: String, quoteChar)
        val unescape = unescapeHexQuote compose unescapeUnicodeQuote
        super.walk(StringLiteral(info, quote, unescape(str), false))

      // Pattern match on calls to the "unescape" function. This function takes
      // a single string argument. Only string literals are handled here
      // however other deobfuscation stages (e.g. constant propagation) may
      // replace a variable argument with a string literal.
      case FunApp(info, VarRef(_, Id(_, "unescape", _, _)),
        StringLiteral(_, quote, str, false) :: Nil) if quote.length == 1 =>
        super.walk(StringLiteral(info, quote, unescapeUri(str, quote.charAt(0)), false))

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node)
        if (newNode != node) walk(newNode) else newNode
    }
  }

  ////////////////////////////////////////////////////////////////
  // calculate results
  ////////////////////////////////////////////////////////////////

  (result, excLog)
}
