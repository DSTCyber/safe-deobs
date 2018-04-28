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
