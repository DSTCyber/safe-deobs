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
import kr.ac.kaist.safe.nodes.ast._

import scala.collection.mutable

/**
 * Deletes unused variables.
 *
 * Note that variables must be declared (i.e. using `var`) to be deleted.
 */
class UnusedVariableRemover(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = UnusedVariableRemovalWalker.walk(program, new Env)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  private type IdStack = mutable.ArrayStack[mutable.Set[Id]]

  /**
   * Environment for recording for recording which variables are removable.
   *
   * The environment is a stack of variable identifiers, where each stack frame
   * represents a new scope where variable declarations can occur. Due to
   * hoisting, variable declarations can only occur at the top level and in
   * function declarations. When a new scope is entered (e.g. due to nested
   * function declarations), a new set of variables is pushed onto the stack.
   * When we leave that scope, the set of variables is destroyed.
   *
   * There should always be at least one frame in the stack. This corresponds
   * to the top level (global) scope and is created by the `TopLevel` AST
   * element.
   */
  private class Env(val ids: IdStack = mutable.ArrayStack()) {
    /**
     * Enter a new scope.
     *
     * This pushes an empty variable mapping into the environment.
     */
    def enterScope(ast: ASTNode): Unit =
      ids.push(mutable.Set())

    /**
     * Exit a scope.
     *
     * This destroys the top (i.e. most recent) variable mapping in the
     * environment.
     */
    def exitScope(ast: ASTNode): Unit =
      ids.pop()

    /**
     * Add a variable to the environment.
     */
    def addVar(id: Id): Unit =
      ids.head += id

    /**
     * Remove a variable from the environment.
     */
    def removeVar(id: Id): Unit = ids.find(_.exists(_ =~ id)) match {
      case Some(s) => s.retain(i => !(i =~ id))
      case None => ()
    }

    /**
     * Returns `true` if the given variable has been declared in the current
     * scope.
     */
    def varDeclared(id: Id): Boolean =
      ids.head.exists(_ =~ id)

    /**
     * Gets the set of variables to delete in the current scope.
     */
    def varsToRemove(): Set[Id] =
      Set(ids.head.toList: _*)
  }

  private object UnusedVariableRemovalWalker extends ASTEnvWalker[Env] {
    override def walk(node: TopLevel, env: Env): TopLevel = node match {
      case TopLevel(info, fds, vds, stmts) =>
        // Create the top level stack frame for all global variables and
        // functions
        env.enterScope(node)
        // Walk the global variable declarations. This will add new variables
        // to the environment. By default we assume that these variables will
        // be deleted unless a reference to a variable is encountered 
        val newVds = vds.map(walk(_, env))
        // Walk the global function declarations and remove any unused
        // variables in the function
        val newFds = fds.map(walk(_, env))
        // Determine which variables can be removed in the top level code
        val newStmts = stmts.map(walk(_, env))
        // Remove unused top level variable declarations
        val varsToRemove = env.varsToRemove
        val filteredVds = newVds.filterNot(vd => varsToRemove.contains(vd.name))
        // Rewalk the AST and remove any assignments to removed variables
        val filteredStmts = newStmts.map(new RemoveAssignmentsWalker(varsToRemove).walk(_))
        // We're finished - destroy this stack frame
        env.exitScope(node)
        TopLevel(info, newFds, filteredVds, filteredStmts)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: Functional, env: Env): Functional = node match {
      case Functional(info, fds, vds, SourceElements(seInfo, seBody, strict), name, params, body) =>
        // Create a new environment stack frame for this function
        env.enterScope(node)
        // Walk the local variable declarations. This will add new variables to
        // the environment. By default we assume that these variables will be
        // deleted unless a reference to a variable is encountered
        val newVds = vds.map(walk(_, env))
        // Walk the local function declarations. This will recursively create a
        // new environment stack frame and remove unused variables in the
        // function
        val newFds = fds.map(walk(_, env))
        // Determine which variables can be removed from this function
        val newSeBody = seBody.map(walk(_, env))
        // Remove unused variable declarations from this function
        val varsToRemove = env.varsToRemove
        val filteredVds = newVds.filterNot(vd => varsToRemove.contains(vd.name))
        // Rewalk the function's AST and remove any assignments to removed
        // variables
        val filteredSeBody = newSeBody.map(new RemoveAssignmentsWalker(varsToRemove).walk(_))
        // We're finished - destroy this stack frame
        env.exitScope(node)
        Functional(info, newFds, filteredVds, SourceElements(seInfo, filteredSeBody, strict), name, params, body)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: VarDecl, env: Env): VarDecl = node match {
      // Add a variable to the environment.
      case VarDecl(_, name, _, _) =>
        env.addVar(name)
        node
    }

    override def walk(node: Expr, env: Env): Expr = node match {
      // We don't want to recurse and include the LHS (i.e. the assigned
      // variable) as a referenced variable.
      //
      // We can still assign to variables and never use them, making them
      // removable.
      case AssignOpApp(info, vr @ VarRef(_, id), op, right) =>
        // It is it necessary to declare a variable (with val) before using it.
        // Therefore we check that all assigned variables have been declared.
        // If a variable has not been declared, add it to the environment in
        // the current scope (if that variable is later referenced it will be
        // removed from the environment and not deleted).
        if (!env.varDeclared(id)) env.addVar(id)
        AssignOpApp(info, vr, op, walk(right, env))

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: LHS, env: Env): LHS = node match {
      // If we see a variable being referenced, we cannot remove it. Update
      // the environment with this information
      case VarRef(_, id) =>
        env.removeVar(id)
        node

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    private class RemoveAssignmentsWalker(varIds: Set[Id]) extends ASTWalker {
      /** Check if the given variable is removable. */
      private def isRemovable(id: Id): Boolean =
        varIds.exists(_ =~ id)

      override def walk(node: Expr): Expr = node match {
        // If we are assigning a value to a variable that is removable, replace
        // the assignment with an empty expression.
        case AssignOpApp(info, VarRef(_, id), _, _) if isRemovable(id) =>
          EmptyExpr(info)

        // Rewalk the node if a change has been made to the AST
        case _ =>
          val newNode = super.walk(node)
          if (newNode != node) walk(newNode) else newNode
      }
    }
  }
}
