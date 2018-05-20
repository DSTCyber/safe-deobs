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
import scala.annotation.tailrec

/**
 * Removes dead (i.e. unexecutable) branches in \c if statements when the
 * branch condition is a constant value.
 */
class DeadBranchRemover(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = DeadBranchRemovalWalker.walk(program)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  private object DeadBranchRemovalWalker extends ASTWalker {
    /**
     * Evaluates a literal (i.e. constant) expression to a boolean value, as
     * per Section 7.1.2 of ECMA-262 edition 8.
     *
     * @param cond The conditional expression
     * @return The result of the \c toBoolean evaluation
     */
    private def evalLiteralCondition(cond: Literal): Boolean = cond match {
      case _: This => true
      case _: Null => false
      case _: Undefined => false
      case Bool(_, bool) => bool
      case DoubleLiteral(_, _, num) => num != 0.0
      case IntLiteral(_, intVal, _) => intVal.compareTo(BigInteger.ZERO) != 0
      case StringLiteral(_, _, escaped, false) => escaped != ""
      // A regular expression cannot be empty, so it always evaluates to true
      case _: RegularExpression => true
    }

    /**
     * Flatten a list of case statements into just their body (a list of
     * statements), starting from the first case statement up to and including
     * the first case statement that contains a break statement. This is done
     * to account for "fall through" cases.
     *
     * E.g. If we searched for the condition "10" in the following code
     * snippet only the body of the 2nd case statement would be returned
     * (because it contains a break statement):
     *
     * \code{.js}
     * switch (0) {
     *   case 10:
     *     break;
     *   case 0:
     *     a = 10;
     *     break;
     *   case 1:
     *     break;
     * }
     * \endcode
     *
     * However, this code snippet would return the 2nd and 3rd cases (because
     * the 2nd case statement doesn't contain a break and hence "falls
     * through"):
     *
     * \code{.js}
     * switch (0) {
     *   case 10:
     *     break;
     *   case 0:
     *     a = 10;
     *   case 1:
     *     b = 11;
     *     break;
     * }
     * \endcode
     *
     * Return a flattened list of statements. If the list of statements
     * contains a `break` statement, only include up to (and including) the
     * `break` (everthign after is redundant).
     */
    private def flattenCases(cases: List[Case]): List[Stmt] = {
      // The case statement that we are interested in might not end with a
      // break statement (i.e. it "falls through" to the next case statement),
      // so we need to handle this possibility.
      //
      // Flatten the list of case statements up until and including the first
      // case statement that ends with a break statement. Anything following
      // the break statement is discarded.
      @tailrec
      def doFlatten(cases: List[Case], accum: List[Stmt]): List[Stmt] =
        cases match {
          case c :: cs =>
            val idx = breakIndex(c.body)
            if (idx == -1) {
              // No break statement - recurse
              doFlatten(cs, accum ++ c.body)
            } else {
              // Make sure we include the break statement
              accum ++ c.body.slice(0, idx + 1)
            }
          case Nil => accum
        }

      doFlatten(cases, Nil)
    }

    /**
     * Returns the index of a break statement in a list of break statements, or
     * -1 if no break statement exists.
     */
    private def breakIndex(stmts: List[Stmt]): Int = stmts.indexWhere {
      case _: Break => true
      case _ => false
    }

    /**
     * Returns true if all of the cases contain a literal condition expression.
     */
    private def allLiteralConds(cases: List[Case]): Boolean = cases.forall {
      case Case(_, _: Literal, _) => true
      case _ => false
    }

    override def walk(node: Stmt): Stmt = node match {
      // If the condition is a literal (i.e. constant), evaluate it and remove
      // one of the branches depending on the result.
      //
      // If the condition evaluates to false and there is no false branch, then
      // we can replace the entire if statement with an empty one.
      case If(info, cond: Literal, trueBranch, falseBranch) =>
        if (evalLiteralCondition(cond)) {
          trueBranch
        } else {
          falseBranch match {
            case Some(stmt) => stmt
            case None => EmptyStmt(info)
          }
        }

      // TODO handle back cases
      case Switch(info, cond: Literal, frontCases, defopt, Nil) if allLiteralConds(frontCases) =>
        // dropWhile will drop case statements until we find one that matches
        // the given condition
        def dropUntilCondFound(cs: List[Case]): List[Case] =
          cs.dropWhile(c => !(c.cond =~ cond))

        // Check if the given list of statements ends with a break statement.
        def endsWithBreak(stmts: List[Stmt]): Boolean = stmts.lastOption match {
          case Some(_: Break) => true
          case _ => false
        }

        // Look through the front statements until we find one that matches the
        // literal condition expression. This will return a list of case
        // statements that includes (a) the case statement that matches the
        // given condition and (b) all the following case statements. We need
        // to include the following case statements in case (a) doesn't end in
        // a break
        val newFrontCases = dropUntilCondFound(frontCases)
        // Flatten the reduced list of case statements, starting at the case
        // statement that matches the given condition. This process will
        // iterate and merge the case statements until a break statement is
        // found.
        flattenCases(newFrontCases) match {
          // The flattened list of statements ends with a break statement. This
          // means we can safely wipe out the rest of the switch statement
          // (because there is no "fall through") and replace it with the
          // flattened statements.
          //
          // Drop the break statement at the end (it is redundant).
          case frontStmts if endsWithBreak(frontStmts) =>
            ABlock(info, frontStmts.init, false)
          // The flattened list of statements is either empty or doesn't end
          // with a break statement. In either case, we must look to the
          // default case.
          //
          // If a default case exists, then append it to the flattened front
          // statement. If there is no default statement, then there is nothing
          // for the front cases to "fall through" to and we just return the
          // front cases.
          case frontStmts => defopt match {
            case Some(defStmts) if endsWithBreak(defStmts) =>
              ABlock(info, frontStmts ++ defStmts.init, false)
            case Some(defStmts) =>
              ABlock(info, frontStmts ++ defStmts, false)
            case None =>
              ABlock(info, frontStmts, false)
          }
        }

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
