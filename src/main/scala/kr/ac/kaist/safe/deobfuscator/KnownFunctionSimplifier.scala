/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * Copyright (c) 2018, DST Group.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.deobfuscator

import java.math.BigInteger

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.ast._

/**
 * Simplify commonly used functions from the JavaScript API.
 */
class KnownFunctionSimplifier(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = KnownFunctionsWalker.walk(program)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  /**
   * Returns true if all the expressions are integer literals.
   */
  private def allIntLiterals(exprs: List[Expr]): Boolean =
    exprs.forall {
      case _: IntLiteral => true
      case _ => false
    }

  private object KnownFunctionsWalker extends ASTWalker {
    override def walk(node: Expr): Expr = node match {
      // Coalesce a number of Unicode numbers into a single string
      case FunApp(info, Dot(_, VarRef(_, Id(_, "String", _, _)), Id(_, "fromCharCode", _, _)), args) if allIntLiterals(args) =>
        val str = args.foldLeft("") {
          case (str, arg: IntLiteral) => str :+ arg.intVal.intValue.toChar
          // TODO Throw exception (although this should never happen)
          case _ => ???
        }
        StringLiteral(info, "'", str, false)

      // Determine the position of the first occurance of a specified value in a
      // string
      case FunApp(info, Dot(_, StringLiteral(_, _, str, false), Id(_, "indexOf", _, _)),
        List(StringLiteral(_, _, searchValue, false))) =>
        val index = str.indexOf(searchValue)
        IntLiteral(info, BigInteger.valueOf(index), 10)
      case FunApp(info, Dot(_, StringLiteral(_, _, str, false), Id(_, "indexOf", _, _)),
        List(StringLiteral(_, _, searchValue, false), IntLiteral(_, start, _))) =>
        val index = str.indexOf(searchValue, start.intValue)
        IntLiteral(info, BigInteger.valueOf(index), 10)

      // Calculate string length
      case Dot(info, StringLiteral(_, _, str, false), Id(_, "length", _, _)) =>
        IntLiteral(info, BigInteger.valueOf(str.length), 10)

      // Determine the character at a specific index of a string
      case FunApp(info, Dot(_, StringLiteral(_, quote, str, false), Id(_, "charAt", _, _)),
        List(IntLiteral(_, intVal, _))) =>
        val chr = str.charAt(intVal.intValue)
        StringLiteral(info, quote, chr.toString, false)

      // Replace a literal substring in a literal string
      case FunApp(info, Dot(_, StringLiteral(_, quote, str, false), Id(_, "replace", _, _)),
        List(StringLiteral(_, _, substr, false), StringLiteral(_, _, newSubstr, false))) =>
        StringLiteral(info, quote, str.replace(substr, newSubstr), false)

      // Convert a number to a string. Note that number literals must be
      // parenthesized (unlike booleans)
      case FunApp(info, Dot(_, Bool(_, bool), Id(_, "toString", _, _)), Nil) =>
        StringLiteral(info, "'", bool.toString, false)
      case FunApp(info, Dot(_, Parenthesized(_, DoubleLiteral(_, _, num)), Id(_, "toString", _, _)), Nil) =>
        StringLiteral(info, "'", num.toString, false)
      case FunApp(info, Dot(_, Parenthesized(_, IntLiteral(_, intVal, _)), Id(_, "toString", _, _)), Nil) =>
        StringLiteral(info, "'", intVal.toString, false)

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
