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

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.nodes.ast._

/**
 * Performs function inlining on an AST.
 *
 * Only trivially simple functions (i.e. those with a single <tt>return</tt>
 * statement) are inlined.
 */
class FunctionInliner(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = FunctionInlineWalker.walk(program, Map[Id, Option[Expr]]())

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  private type Env = Map[Id, Option[Expr]]

  private object FunctionInlineWalker extends ASTEnvWalker[Env] {
    /**
     * Generate a map of inlinable functions.
     *
     * A function is inlinable if it only contains a single <tt>return</tt>
     * statement, which we can trivially inline.
     *
     * The map associates the name of an inlinable function with the value
     * that calls to this function will be replaced with.
     *
     * @param funcs A list of functions
     * @return A map of inlinable functions and the expression that we will
     *         inline them
     */
    private def getInlinableFunctions(funcs: List[Functional]): Env =
      funcs.foldLeft(Map[Id, Option[Expr]]())((m, func) => {
        val body = func.stmts.body
        // We can only inline functions with a single return statement
        if (body.length == 1) {
          body.last match {
            // If the function returns a literal expression, we can inline it
            // with that literal expression
            case Return(_, lit @ Some(_: Literal)) => m + (func.name -> lit)
            // If the function consists of an empty return statement, we can
            // just delete it
            case Return(_, None) => m + (func.name -> None)
            // The function is too complex to inline if it returns something
            // more than a literal expression
            case _ => m
          }
        } else {
          // The function has too many statements and therefore cannot be
          // inlined
          m
        }
      })

    override def walk(node: TopLevel, env: Env): TopLevel = node match {
      case TopLevel(info, fds, vds, stmts) =>
        // Find functions that we can inline
        val newEnv = env ++ getInlinableFunctions(fds.map(_.ftn))
        // Inline functions by walking all of the top-level functions,
        // variable declarations and statements and replacing each
        // function call with an inlined version of the function
        val newFds = fds.map(walk(_, newEnv))
        val newVds = vds.map(walk(_, newEnv))
        val newStmts = stmts.map(walk(_, newEnv))
        // Delete inlined functions by filtering them out of the list of
        // function definitions
        val filteredFds = newFds.filterNot(fd => newEnv.keySet.contains(fd.ftn.name))
        TopLevel(info, filteredFds, newVds, newStmts)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: Expr, env: Env): Expr = node match {
      // A function call. Check if it is inlinable, and inline it as
      // appropriate.
      case FunApp(info, vr @ VarRef(_, id), args) =>
        env.find { case (name, _) => name =~ id } match {
          // An inlinable function that we can replace with the given
          // expression (taken from the function's return statement).
          case Some((_, Some(expr))) => expr
          // An inlinable, empty function. Just replace the function call with
          // an empty expression
          case Some((_, None)) => EmptyExpr(info)
          // If the function is not inlinable, walk the function argument
          // expressions (because they might be inlinable function calls) and
          // return the function call expression (with the new arguments).
          case None =>
            val newArgs = args.map(walk(_, env))
            FunApp(info, vr, newArgs)
        }

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }
  }

  ////////////////////////////////////////////////////////////////
  // calculate results
  ////////////////////////////////////////////////////////////////

  (result, excLog)
}
