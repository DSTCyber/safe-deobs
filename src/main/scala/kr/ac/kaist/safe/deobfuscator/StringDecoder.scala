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
     * Hex-encoded characters are converted to ASCII values when possible.
     * Quote (" and ') characters within the character sequence must be escaped
     * so that the string can be re-terminated.
     */
    private def unescapeHex(seq: Seq[Char]): Seq[Char] = seq match {
      case Seq('\\', x1, x2, x3, xs @ _*) if Character.toLowerCase(x1) == 'x' &&
        Character.digit(x2, 16) != -1 &&
        Character.digit(x3, 16) != -1 =>
        val c = combineHexChars(x2, x3)
        val pre = if (c == '"' || c == '\'') Seq('\\', c) else Seq(c)
        pre ++ unescapeHex(xs)
      case Seq(x, xs @ _*) => x +: unescapeHex(xs)
      case Seq() => ""
    }

    private def unescapeHex(str: String): String = unescapeHex(str.toSeq).mkString

    /**
     * URI-encoded characters are converted to ASCII values when possible.
     * Quote (") characters within the character sequence must be escaped so
     * that the string can be re-terminated.
     */
    private def unescapeUri(seq: Seq[Char]): Seq[Char] = seq match {
      case Seq('%', x1, x2, xs @ _*) if Character.digit(x1, 16) != -1 &&
        Character.digit(x2, 16) != -1 => combineHexChars(x1, x2) +: unescapeUri(xs)
      case Seq(x, xs @ _*) => x +: unescapeUri(xs)
      case Seq() => ""
    }

    private def unescapeUri(str: String): String = unescapeUri(str.toSeq).mkString

    private def combineHexChars(x1: Char, x2: Char): Char =
      (Character.digit(x1, 16) * 16 + Character.digit(x2, 16)).toChar

    override def walk(node: LHS): LHS = node match {
      case StringLiteral(info, quote, str, false) =>
        super.walk(StringLiteral(info, quote, unescapeHex(str), false))
      // Pattern match on calls to the "unescape" function. This function takes
      // a single string argument. Only string literals are handled here
      // however other deobfuscation stages (e.g. constant propagation) may
      // replace a variable argument with a string literal.
      case FunApp(info, VarRef(_, Id(_, "unescape", _, _)), StringLiteral(_, quote, str, false) :: Nil) =>
        super.walk(StringLiteral(info, quote, unescapeUri(str), false))
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
