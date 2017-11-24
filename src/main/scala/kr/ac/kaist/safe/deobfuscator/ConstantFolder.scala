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

import java.math.BigInteger

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.util.{ NodeUtil => NU }

/**
 * Performs constant folding on an AST.
 */
class ConstantFolder(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = ConstantFoldWalker.walk(program)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  private object ConstantFoldWalker extends ASTWalker {
    /**
     * Determine whether an operator is a unary operator, as defined in Section
     * 12.5 of ECMA-262 edition 8.
     */
    private def isUnaryOp(op: Op): Boolean = op match {
      case Op(_, "delete") => true
      case Op(_, "void") => true
      case Op(_, "typeof") => true
      case Op(_, "+") => true
      case Op(_, "-") => true
      case Op(_, "~") => true
      case Op(_, "!") => true
      case _ => false
    }

    /**
     * Determine whether an operator is an exponentiation operator, as defined
     * in Section 12.6 of ECMA-262 edition 8.
     */
    private def isExponentiationOp(op: Op): Boolean = op match {
      case Op(_, "**") => true
      case _ => false
    }

    /**
     * Determine whether an operator is a multiplicative operator, as defined
     * in Section 12.7 of ECMA-262 edition 8.
     */
    private def isMultiplicativeOp(op: Op): Boolean = op match {
      case Op(_, "*") => true
      case Op(_, "/") => true
      case Op(_, "%") => true
      case _ => false
    }

    /**
     * Determine whether an operator is an applicative operator, as defined in
     * Section 12.8 of ECMA-262 edition 8.
     */
    private def isAdditiveOp(op: Op): Boolean = op match {
      case Op(_, "+") => true
      case Op(_, "-") => true
      case _ => false
    }

    /**
     * Determine whether an operator is a bitwise shift operator, as defined
     * in Section 12.9 of ECMA-262 edition 8.
     */
    private def isBitwiseShiftOperator(op: Op): Boolean = op match {
      case Op(_, "<<") => true
      case Op(_, ">>") => true
      case Op(_, ">>>") => true
      case _ => false
    }

    /**
     * Determine whether an operator is a relational operator, as defined in
     * Section 12.10 of ECMA-262 edition 8.
     */
    private def isRelationalOperator(op: Op): Boolean = op match {
      case Op(_, "<") => true
      case Op(_, ">") => true
      case Op(_, "<=") => true
      case Op(_, ">=") => true
      case Op(_, "instanceof") => true
      case Op(_, "in") => true
      case _ => false
    }

    /**
     * Determine whether an operator is an equality operator, as defined in
     * Section 12.11 of ECMA-262 edition 8.
     */
    private def isEqualityOperator(op: Op): Boolean = op match {
      case Op(_, "==") => true
      case Op(_, "!=") => true
      case Op(_, "===") => true
      case Op(_, "!==") => true
      case _ => false
    }

    /**
     * Determine whether an operator is a binary bitwise operator, as defined
     * in Section 12.12 of ECMA-262 edition 8.
     */
    private def isBinaryBitwiseOp(op: Op): Boolean = op match {
      case Op(_, "&") => true
      case Op(_, "|") => true
      case Op(_, "^") => true
      case _ => false
    }

    /**
     * Determine whether an operator is a binary logical operator, as defined
     * in Section 12.13 of ECMA-262 edition 8.
     */
    private def isBinaryLogicalOp(op: Op): Boolean = op match {
      case Op(_, "&&") => true
      case Op(_, "||") => true
      case _ => false
    }

    /** Returns \c true if the operator is a binary arithmetic operator. */
    private def isBinaryArithmeticOp(op: Op): Boolean =
      isMultiplicativeOp(op) || isAdditiveOp(op) || isBitwiseShiftOperator(op)

    /** Returns \c true if the operation is a comparison operator. */
    private def isComparisonOp(op: Op): Boolean =
      isRelationalOperator(op) || isEqualityOperator(op)

    /**
     * Converts a JavaScript binary arithmetic operation to a \c BigInteger
     * method.
     */
    private def opToIntArithmeticFunc(op: Op): Option[(BigInteger, BigInteger) => BigInteger] = op match {
      case Op(_, "+") => Some((x, y) => x.add(y))
      case Op(_, "-") => Some((x, y) => x.subtract(y))
      case Op(_, "*") => Some((x, y) => x.multiply(y))
      case Op(_, "%") => Some((x, y) => x.mod(y))
      case Op(_, "&") => Some((x, y) => x.and(y))
      case Op(_, "|") => Some((x, y) => x.or(y))
      case Op(_, "^") => Some((x, y) => x.xor(y))
      case Op(_, "<<") => Some((x, y) => x.shiftLeft(y.intValue))
      case Op(_, ">>") => Some((x, y) => x.shiftRight(y.intValue))
      case _ => None
    }

    /**
     * Converts a JavaScript integer comparison operation to a \c BigInteger
     * method.
     */
    private def opToIntComparisonFunc(op: Op): Option[(BigInteger, BigInteger) => Boolean] = op match {
      case Op(_, "<") => Some((x, y) => x.compareTo(y) < 0)
      case Op(_, "==") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, ">") => Some((x, y) => x.compareTo(y) > 0)
      case Op(_, ">=") => Some((x, y) => x.compareTo(y) >= 0)
      case Op(_, "!=") => Some((x, y) => x.compareTo(y) != 0)
      case Op(_, "<=") => Some((x, y) => x.compareTo(y) <= 0)
      case Op(_, "===") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, "!==") => Some((x, y) => x.compareTo(y) != 0)
      case _ => None
    }

    /**
     * Converts a JavaScript string comparison operation to a \c String method.
     */
    private def opToStringComparisonFunc(op: Op): Option[(String, String) => Boolean] = op match {
      case Op(_, "==") => Some((x, y) => x == y)
      case Op(_, "!=") => Some((x, y) => x != y)
      case Op(_, "===") => Some((x, y) => x == y)
      case Op(_, "!==") => Some((x, y) => x != y)
      case _ => None
    }

    /**
     * Converts a JavaScript double binary arithmetic operation to a \c Double
     * method.
     */
    private def opToDoubleArithmeticFunc(op: Op): Option[(Double, Double) => Double] = op match {
      case Op(_, "+") => Some((x, y) => x.doubleValue + y.doubleValue)
      case Op(_, "-") => Some((x, y) => x.doubleValue - y.doubleValue)
      case Op(_, "*") => Some((x, y) => x.doubleValue * y.doubleValue)
      case Op(_, "/") => Some((x, y) => x.doubleValue / y.doubleValue)
      case _ => None
    }

    /**
     * Converts a JavaScript double comparison operation to a \c Double method.
     */
    // TODO reduce code duplication with opToIntComparisonFunc
    private def opToDoubleComparisonFunc(op: Op): Option[(Double, Double) => Boolean] = op match {
      case Op(_, "<") => Some((x, y) => x.compareTo(y) < 0)
      case Op(_, "==") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, ">") => Some((x, y) => x.compareTo(y) > 0)
      case Op(_, ">=") => Some((x, y) => x.compareTo(y) >= 0)
      case Op(_, "!=") => Some((x, y) => x.compareTo(y) != 0)
      case Op(_, "<=") => Some((x, y) => x.compareTo(y) <= 0)
      case Op(_, "===") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, "!==") => Some((x, y) => x.compareTo(y) != 0)
      case _ => None
    }

    /**
     * Implicitly converts a \c Boolean to a \c Long - just like JavaScript!
     */
    private implicit def boolToInt(b: Boolean): Long = if (b) 1 else 0

    override def walk(node: Expr): Expr = node match {
      // Simplifies a binary arithmetic operation (e.g. addition, subtraction,
      // etc.) on two integer literals into a single integer literal.
      //
      // SAFE uses BigInteger objects to represent integer literals, so we must
      // map the SAFE operator to a BigInteger method that performs the same
      // operation (i.e. preserves its semantics). This mapping is performed by
      // the opToIntArithmeticFunc method. If no corresponding BigInteger
      // method exists, the AST node is returned unchanged.
      //
      // NOTE that this does not handle divide, because divide may return
      // either an integer or double depending on the result.
      case InfixOpApp(info, IntLiteral(_, left, radix), op, IntLiteral(_, right, _)) if isBinaryArithmeticOp(op) =>
        opToIntArithmeticFunc(op) match {
          case Some(opFunc) => IntLiteral(info, opFunc(left, right), radix)
          case None => node
        }

      // Simplifies a comparison operation (e.g. less than, equality, etc.) on
      // two integer literals into a single boolean literal.
      //
      // SAFE uses BigInteger objects to represent integer literals, so we must
      // map the SAFE operator to a BigInteger method that performs the same
      // operation (i.e. preserves its semantics). This mapping is performed by
      // the opToIntComparisonFunc method. If no corresponding BigInteger
      // method exists, the AST node is returned unchanged.
      case InfixOpApp(info, IntLiteral(_, left, _), op, IntLiteral(_, right, _)) if isComparisonOp(op) =>
        opToIntComparisonFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

      // Simplifies a binary arithmetic operation (e.g. addition, subtraction,
      // etc.) on two double literals into a single double literal.
      //
      // SAFE uses Double objects to represent double literals, so we must map
      // the SAFE operator to a Double method that performs the same operation
      // (i.e. preserves its semantics). This mapping is performed by the
      // opToDoubleArithmeticFunc method. If no corresponding Double method
      // exists, the AST node is returned unchanged.
      case InfixOpApp(info, DoubleLiteral(_, _, left), op, DoubleLiteral(_, _, right)) if isBinaryArithmeticOp(op) =>
        opToDoubleArithmeticFunc(op) match {
          case Some(opFunc) =>
            val result = opFunc(left, right)
            DoubleLiteral(info, result.toString, result)
          case None => node
        }

      // Simplifies a comparison operation (e.g. less than, equality, etc.) on
      // two double literals into a single boolean literal.
      //
      // SAFE uses Double objects to represent double literals, so we must map
      // the SAFE operator to a Double method that performs the same operation
      // (i.e. preserves its semantics). This mapping is performed by the
      // opToDoubleComparisonFunc method. If no corresponding Double method
      // exists, the AST node is returned unchanged.
      case InfixOpApp(info, DoubleLiteral(_, _, left), op, DoubleLiteral(_, _, right)) if isComparisonOp(op) =>
        opToDoubleComparisonFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

      // Simplifies a comparison operation (equality) on two string literals
      // into a single boolean literal.
      //
      // We need to convert SAFE's Op class to a comparison method that
      // operates on Scala strings. If no corresponding method exists, the AST
      // node is returned unchanged.
      case InfixOpApp(info, StringLiteral(_, _, left, false), op, StringLiteral(_, _, right, false)) if isComparisonOp(op) =>
        opToStringComparisonFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

      // Simplifies the concatenation of two string literals into a single
      // string literal
      case InfixOpApp(info, StringLiteral(_, quote, left, false), Op(_, "+"), StringLiteral(_, _, right, false)) =>
        StringLiteral(info, quote, left + right, false)

      // Simplifies the concatenation of a string literal with an integer
      // literal into a single string literal
      case InfixOpApp(info, StringLiteral(_, quote, str, false), Op(_, "+"), IntLiteral(_, int, _)) =>
        StringLiteral(info, quote, s"${str}${int}", false)
      case InfixOpApp(info, IntLiteral(_, int, _), Op(_, "+"), StringLiteral(_, quote, str, false)) =>
        StringLiteral(info, quote, s"${int}${str}", false)

      // Simplifies the concatenation of a string literal with a boolean
      // literal into a single string literal. This uses Scala's string
      // representation of bools, which coincidentally is exactly the same as
      // JavaScript's.
      case InfixOpApp(info, StringLiteral(_, quote, str, false), Op(_, "+"), Bool(_, bool)) =>
        StringLiteral(info, quote, s"${str}${bool}", false)
      case InfixOpApp(info, Bool(_, bool), Op(_, "+"), StringLiteral(_, quote, str, false)) =>
        StringLiteral(info, quote, s"${bool}${str}", false)

      // Simplifies a binary arithmetic operation (e.g. addition, subtraction,
      // etc.) on an integer and boolean into an integer literal.
      //
      // When a binary operation is performed on an integer and a boolean, the
      // boolean is coerced into an integer as follows:
      //
      //   false => 0
      //   true  => 1
      //
      // This is done implicitly with the boolToInt method.
      case InfixOpApp(info, IntLiteral(_, int, radix), op, Bool(_, bool)) =>
        opToIntArithmeticFunc(op) match {
          case Some(opFunc) => IntLiteral(info, opFunc(int, BigInteger.valueOf(bool)), radix)
          case None => node
        }
      case InfixOpApp(info, Bool(_, bool), op, IntLiteral(_, int, radix)) =>
        opToIntArithmeticFunc(op) match {
          case Some(opFunc) => IntLiteral(info, opFunc(BigInteger.valueOf(bool), int), radix)
          case None => node
        }

      // Simplify a binary operation in a single parenthesized expression by
      // "unwrapping" the parenthesized expression
      case InfixOpApp(info, paren: Parenthesized, op, right) =>
        walk(InfixOpApp(info, walk(paren.unwrapParen), op, walk(right)))
      case InfixOpApp(info, left, op, paren: Parenthesized) =>
        walk(InfixOpApp(info, walk(left), op, walk(paren.unwrapParen)))

      // Simplify a binary operation on an expression list. When performing a
      // binary operation on an expression list, only the last expression is
      // looked at
      case InfixOpApp(info, ExprList(_, exprs), op, right) =>
        walk(InfixOpApp(info, walk(exprs.last), op, walk(right)))
      case InfixOpApp(info, left, op, ExprList(_, exprs)) =>
        walk(InfixOpApp(info, walk(left), op, walk(exprs.last)))

      // Simplifies the inversion of a boolean literal
      case PrefixOpApp(info, Op(_, "!"), Bool(_, bool)) =>
        Bool(info, !bool)

      // Simplifies the inversion of an integer literal. Returns true iff the
      // integer is zero
      case PrefixOpApp(info, Op(_, "!"), IntLiteral(_, int, _)) =>
        int match {
          case BigInteger.ZERO => Bool(info, true)
          case _ => Bool(info, false)
        }

      // Simplifies the inversion of a string literal. Returns true iff the
      // string is empty
      case PrefixOpApp(info, Op(_, "!"), StringLiteral(_, _, str, false)) =>
        str match {
          case "" => Bool(info, true)
          case _ => Bool(info, false)
        }

      // Simplifies the inversion of a double literal. Returns true iff the
      // double is 0
      case PrefixOpApp(info, Op(_, "!"), DoubleLiteral(_, _, double)) =>
        double.doubleValue match {
          case 0.0 => Bool(info, true)
          case _ => Bool(info, false)
        }

      // Simplifies the typeof a literal expression. Returns the result defined
      // in Table 35 of ECMA-262 edition 8
      case PrefixOpApp(info, Op(_, "typeof"), Undefined(_)) =>
        StringLiteral(info, "\"", "undefined", false)

      case PrefixOpApp(info, Op(_, "typeof"), Null(_)) =>
        StringLiteral(info, "\"", "object", false)

      case PrefixOpApp(info, Op(_, "typeof"), Bool(_, _)) =>
        StringLiteral(info, "\"", "boolean", false)

      case PrefixOpApp(info, Op(_, "typeof"), DoubleLiteral(_, _, _)) =>
        StringLiteral(info, "\"", "number", false)

      case PrefixOpApp(info, Op(_, "typeof"), IntLiteral(_, _, _)) =>
        StringLiteral(info, "\"", "number", false)

      case PrefixOpApp(info, Op(_, "typeof"), StringLiteral(_, _, _, _)) =>
        StringLiteral(info, "\"", "string", false)

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