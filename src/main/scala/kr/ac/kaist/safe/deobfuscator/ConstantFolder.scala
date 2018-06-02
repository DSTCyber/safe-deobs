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
    private def isRelationalOp(op: Op): Boolean = op match {
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
    private def isEqualityOp(op: Op): Boolean = op match {
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

    /** Returns `true` if the operator is a binary arithmetic operator. */
    private def isBinaryArithmeticOp(op: Op): Boolean =
      isMultiplicativeOp(op) || isAdditiveOp(op) || isBitwiseShiftOperator(op)

    /**
     * Converts a JavaScript boolean relational operation to a `Boolean`
     * method.
     */
    private def opToBoolRelationalFunc(op: Op): Option[(Boolean, Boolean) => Boolean] = op match {
      case Op(_, "<") => Some((x, y) => x < y)
      case Op(_, ">") => Some((x, y) => x > y)
      case Op(_, ">=") => Some((x, y) => x >= y)
      case Op(_, "<=") => Some((x, y) => x <= y)
      case _ => None
    }

    /**
     * Converts a JavaScript boolean equality operation to a `Boolean` method.
     */
    private def opToBoolEqualityFunc(op: Op): Option[(Boolean, Boolean) => Boolean] = op match {
      case Op(_, "==") => Some((x, y) => x == y)
      case Op(_, "!=") => Some((x, y) => x != y)
      case Op(_, "===") => Some((x, y) => x == y)
      case Op(_, "!==") => Some((x, y) => x != y)
      case _ => None
    }

    /**
     * Converts a JavaScript binary arithmetic operation to a `BigInteger`
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
     * Converts a JavaScript integer relational operation to a `BigInteger`
     * method.
     */
    private def opToIntRelationalFunc(op: Op): Option[(BigInteger, BigInteger) => Boolean] = op match {
      case Op(_, "<") => Some((x, y) => x.compareTo(y) < 0)
      case Op(_, ">") => Some((x, y) => x.compareTo(y) > 0)
      case Op(_, ">=") => Some((x, y) => x.compareTo(y) >= 0)
      case Op(_, "<=") => Some((x, y) => x.compareTo(y) <= 0)
      case _ => None
    }

    /**
     * Converts a JavaScript integer equality operation to a `BigInteger`
     * method.
     */
    private def opToIntEqualityFunc(op: Op): Option[(BigInteger, BigInteger) => Boolean] = op match {
      case Op(_, "==") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, "!=") => Some((x, y) => x.compareTo(y) != 0)
      case Op(_, "===") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, "!==") => Some((x, y) => x.compareTo(y) != 0)
      case _ => None
    }
    /**
     * Converts a JavaScript string equality operation to a `String` method.
     */
    private def opToStringEqualityFunc(op: Op): Option[(String, String) => Boolean] = op match {
      case Op(_, "==") => Some((x, y) => x == y)
      case Op(_, "!=") => Some((x, y) => x != y)
      case Op(_, "===") => Some((x, y) => x == y)
      case Op(_, "!==") => Some((x, y) => x != y)
      case _ => None
    }

    /**
     * Converts a JavaScript double binary arithmetic operation to a `Double`
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
     * Converts a JavaScript double relational operation to a `Double` method.
     */
    // TODO reduce code duplication with opToIntRelationalFunc
    private def opToDoubleRelationalFunc(op: Op): Option[(Double, Double) => Boolean] = op match {
      case Op(_, "<") => Some((x, y) => x.compareTo(y) < 0)
      case Op(_, ">") => Some((x, y) => x.compareTo(y) > 0)
      case Op(_, ">=") => Some((x, y) => x.compareTo(y) >= 0)
      case Op(_, "<=") => Some((x, y) => x.compareTo(y) <= 0)
      case _ => None
    }

    /**
     * Converts a JavaScript double equality operation to a `Double` method.
     */
    // TODO reduce code duplication with opToIntEqualityFunc
    private def opToDoubleEqualityFunc(op: Op): Option[(Double, Double) => Boolean] = op match {
      case Op(_, "==") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, "!=") => Some((x, y) => x.compareTo(y) != 0)
      case Op(_, "===") => Some((x, y) => x.compareTo(y) == 0)
      case Op(_, "!==") => Some((x, y) => x.compareTo(y) != 0)
      case _ => None
    }
    /**
     * Implicitly converts a `Boolean` to a `Long` - just like JavaScript!
     */
    private implicit def boolToInt(b: Boolean): Long = if (b) 1 else 0

    override def walk(node: Expr): Expr = node match {
      // As per Section 7.2.14 of ECMA-262 edition 8, the strict equality
      // operator will return false if the left and right types are different.
      // If they are the same, we can reduce this to an abstract equality
      // operation between two literals (see Section 7.2.13 of ECMA-262 edition
      // 8).
      case InfixOpApp(info, left: Literal, Op(opInfo, "==="), right: Literal) =>
        if (left.getClass != right.getClass) Bool(info, false)
        else walk(InfixOpApp(info, left, Op(opInfo, "=="), right))

      // Simplifies an equality operation on two null literals into a single
      // boolean literal.
      //
      // We can treat the null literals as the same values (it doesn't
      // really matter what their type is - I've just picked bool) and perform
      // a standard equality operation on these two literals.
      case InfixOpApp(info, _: Null, op, _: Null) if isEqualityOp(op) =>
        opToBoolEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(false, false))
          case None => node
        }

      // As per Section 7.2.13 of ECMA-262 edition 8, Null == Undefined and
      // Undefined == Null will always return true (and conversely
      // Null != Undefined will return false).
      //
      // We can achieve the same result by treating the null and undefined
      // literals as having the same value (it doesn't really matter what
      // their type is - I've just picked bool) and perform a standard
      // equality operation on these two literals.
      case InfixOpApp(info, _: Null, op, _: Undefined) if isEqualityOp(op) =>
        opToBoolEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(false, false))
          case None => node
        }
      case InfixOpApp(info, _: Undefined, op, _: Null) if isEqualityOp(op) =>
        opToBoolEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(false, false))
          case None => node
        }

      // Comparing a Null literal to any other type of literal will always
      // return false, as per Section 7.2.13 of ECMA-262 edition 8.
      case InfixOpApp(info, _: Null, op, _: Literal) if isEqualityOp(op) =>
        Bool(info, false)
      case InfixOpApp(info, _: Literal, op, _: Null) if isEqualityOp(op) =>
        Bool(info, false)

      // Simplifies an equality operation on two undefined literals into a
      // single boolean literal.
      //
      // We can treat the undefined literals as the same values (it doesn't
      // really matter what their type is - I've just picked bool) and perform
      // a standard equality operation on these two literals.
      case InfixOpApp(info, _: Undefined, op, _: Undefined) if isEqualityOp(op) =>
        opToBoolEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(false, false))
          case None => node
        }

      // Comparing an Undefined literal to any other type of literal will
      // always return false, as per Section 7.2.13 of ECMA-262 edition 8.
      case InfixOpApp(info, _: Undefined, op, _: Literal) if isEqualityOp(op) =>
        Bool(info, false)
      case InfixOpApp(info, _: Literal, op, _: Undefined) if isEqualityOp(op) =>
        Bool(info, false)

      // Simplifies a binary arithmetic operation (e.g. addition, subtraction,
      // etc.) on two boolean literals into a single integer literal.
      //
      // This is because boolean values are coerced into integer values as
      // follows:
      //
      //   false => 0
      //   true  => 1
      //
      // This is done implicitly with the boolToInt method.
      case InfixOpApp(info, Bool(_, left), op, Bool(_, right)) if isBinaryArithmeticOp(op) =>
        opToIntArithmeticFunc(op) match {
          case Some(opFunc) => IntLiteral(info, opFunc(BigInteger.valueOf(left), BigInteger.valueOf(right)), 10)
          case None => node
        }

      // Simplifies a comparison operation (relational and equality) on two
      // boolean literals into a single boolean literal.
      //
      // We do this by first coercing the boolean literals into integer values
      // as per Section 7.1.3 in ECMA-262 edition 8:
      //
      //   false => 0
      //   true  => 1
      //
      // This is done implicitly with the boolToInt method.
      case InfixOpApp(info, Bool(_, left), op, Bool(_, right)) if isRelationalOp(op) =>
        opToBoolRelationalFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }
      case InfixOpApp(info, Bool(_, left), op, Bool(_, right)) if isEqualityOp(op) =>
        opToBoolEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

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

      // Simplifies a comparison operation (relational and equality) on
      // two integer literals into a single boolean literal.
      //
      // SAFE uses BigInteger objects to represent integer literals, so we must
      // map the SAFE operator to a BigInteger method that performs the same
      // operation (i.e. preserves its semantics). This mapping is performed by
      // the opToInt{Relational,Equality}Func method. If no corresponding
      // BigInteger method exists, the AST node is returned unchanged.
      case InfixOpApp(info, IntLiteral(_, left, _), op, IntLiteral(_, right, _)) if isRelationalOp(op) =>
        opToIntRelationalFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }
      case InfixOpApp(info, IntLiteral(_, left, _), op, IntLiteral(_, right, _)) if isEqualityOp(op) =>
        opToIntEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

      // Simplifies a comparison operation (relational and equality) on an
      // integer an boolean literal into a single boolean literal. The usual
      // coercing of booleans to integers occurs prior to the comparison.
      case InfixOpApp(info, IntLiteral(_, left, _), op, Bool(_, right)) if isRelationalOp(op) =>
        opToIntRelationalFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, BigInteger.valueOf(right)))
          case None => node
        }
      case InfixOpApp(info, IntLiteral(_, left, _), op, Bool(_, right)) if isEqualityOp(op) =>
        opToIntEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, BigInteger.valueOf(right)))
          case None => node
        }
      case InfixOpApp(info, Bool(_, left), op, IntLiteral(_, right, _)) if isRelationalOp(op) =>
        opToIntRelationalFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(BigInteger.valueOf(left), right))
          case None => node
        }
      case InfixOpApp(info, Bool(_, left), op, IntLiteral(_, right, _)) if isEqualityOp(op) =>
        opToIntEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(BigInteger.valueOf(left), right))
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

      // Simplifies a comparison operation (relational and equality) on two
      // double literals into a single boolean literal.
      //
      // SAFE uses Double objects to represent double literals, so we must map
      // the SAFE operator to a Double method that performs the same operation
      // (i.e. preserves its semantics). This mapping is performed by the
      // opToDouble{Relational,Equality}Func method. If no corresponding Double
      // method exists, the AST node is returned unchanged.
      case InfixOpApp(info, DoubleLiteral(_, _, left), op, DoubleLiteral(_, _, right)) if isRelationalOp(op) =>
        opToDoubleRelationalFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }
      case InfixOpApp(info, DoubleLiteral(_, _, left), op, DoubleLiteral(_, _, right)) if isEqualityOp(op) =>
        opToDoubleEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

      // Simplifies an equality operation on two string literals into a single
      // boolean literal.
      //
      // We need to convert SAFE's Op class to a comparison method that
      // operates on Scala strings. If no corresponding method exists, the AST
      // node is returned unchanged.
      case InfixOpApp(info, StringLiteral(_, _, left, false), op, StringLiteral(_, _, right, false)) if isEqualityOp(op) =>
        opToStringEqualityFunc(op) match {
          case Some(opFunc) => Bool(info, opFunc(left, right))
          case None => node
        }

      // Simplifies the concatenation of two string literals into a single
      // string literal
      case InfixOpApp(info, StringLiteral(_, _, left, false), Op(_, "+"), StringLiteral(_, _, right, false)) =>
        // Force the new string to use single quotes, and escape all single
        // quotes that appear in the concatenated string
        val newStr = """'""".r.replaceAllIn(s"$left$right", """\\'""")
        StringLiteral(info, "'", newStr, false)

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

      // Simplifies the concatenation of a string literal with a double literal
      // into a single string literal
      case InfixOpApp(info, StringLiteral(_, quote, str, false), Op(_, "+"), DoubleLiteral(_, _, num)) =>
        StringLiteral(info, quote, s"${str}${num}", false)
      case InfixOpApp(info, DoubleLiteral(_, _, num), Op(_, "+"), StringLiteral(_, quote, str, false)) =>
        StringLiteral(info, quote, s"${num}${str}", false)

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

      // Applies the unary - operator to a boolean literal, which turns the
      // boolean literal into a number and negates it (as described in Section
      // 12.5.7 of ECMA-262 edition 8)
      case PrefixOpApp(info, Op(_, "-"), Bool(_, bool)) =>
        IntLiteral(info, BigInteger.valueOf(bool).negate(), 10)

      // Applies the unary - operator to an integer literal, negating it.
      case PrefixOpApp(info, Op(_, "-"), IntLiteral(_, intVal, radix)) =>
        IntLiteral(info, intVal.negate(), radix)

      // Applies the unary - operator to a double literal, negating it.
      case PrefixOpApp(info, Op(_, "-"), DoubleLiteral(_, text, double)) =>
        DoubleLiteral(info, text, -double)

      // Simplifies the typeof a literal expression. Returns the result defined
      // in Table 35 of ECMA-262 edition 8
      case PrefixOpApp(info, Op(_, "typeof"), _: Undefined) =>
        StringLiteral(info, "\"", "undefined", false)

      case PrefixOpApp(info, Op(_, "typeof"), _: Null) =>
        StringLiteral(info, "\"", "object", false)

      case PrefixOpApp(info, Op(_, "typeof"), _: Bool) =>
        StringLiteral(info, "\"", "boolean", false)

      case PrefixOpApp(info, Op(_, "typeof"), _: DoubleLiteral) =>
        StringLiteral(info, "\"", "number", false)

      case PrefixOpApp(info, Op(_, "typeof"), _: IntLiteral) =>
        StringLiteral(info, "\"", "number", false)

      case PrefixOpApp(info, Op(_, "typeof"), _: StringLiteral) =>
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
