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
import kr.ac.kaist.safe.errors.error._
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.util.{ NodeUtil => NU }

import scala.collection.mutable

/**
 * Performs constant propagation on an AST.
 *
 * Assumes that the \c ConstantFolder phase has been run prior to this phase.
 *
 * The algorithm has been adapted from
 * http://www.cs.tau.ac.il/~msagiv/courses/pa07/lecture2-notes-update.pdf
 */
class ConstantPropagator(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = ConstantPropagationWalker.walk(program, new Env)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  /**
   * Represents the possible abstract values of a variable in our constant
   * propagation domain. This  domain represents the set `Expr ∪ {⊤, ⊥}`.
   */
  private sealed abstract class AbstractValue {
    /**
     * Performs a join (⊔) operation on two abstract values.
     *
     * This code is based on the table on page 5 of
     * http://www.cs.tau.ac.il/~msagiv/courses/pa07/lecture2-notes-update.pdf
     */
    def join(other: AbstractValue): AbstractValue = this match {
      case Top => Top
      case Bottom => other
      case Constant(c1) => other match {
        case Top => Top
        case Bottom => Constant(c1)
        case Constant(c2) => if (c1 =~ c2) Constant(c1) else Top
      }
    }
  }

  /**
   * Top (⊤) indicates that a variable is potentially non-constant.
   */
  private case object Top extends AbstractValue

  /**
   * Bottom (⊥) is the most accurate value that can be assigned. It captures the
   * case where the set of represented states is empty.
   */
  private case object Bottom extends AbstractValue

  /**
   * An abstract representation of a constant JavaScript expression.
   */
  private case class Constant(expr: Expr) extends AbstractValue

  /**
   * Takes a JavaScript expression and transforms it into an abstract value in
   * our constant propagation domain.
   *
   * Literal expressions become constants, and everything else goes to ⊤.
   */
  private def makeAbstract(expr: Expr): AbstractValue = expr match {
    case l: Literal => Constant(l)
    case _ => Top
  }

  /**
   * Mapping between the program's variables and their abstract values in the
   * set <tt>Expr ∪ {⊤, ⊥}</tt>.
   */
  private type VarMap = mutable.Map[String, AbstractValue]

  /**
   * A stack of variable mappings.
   *
   * Each stack frame corresponds to a particular scope in the JavaScript
   * program.
   */
  private type VarStack = mutable.ArrayStack[VarMap]

  /**
   * The constant propagation environment.
   *
   * The environment is a stack of variable mappings, where each stack frame
   * represents a new scope. When a new scope is entered, a new variable
   * mapping is pushed onto the stack. When we leave that scope, the variable
   * mapping is destroyed.
   *
   * There should always be at least one frame in the stack. This frame
   * corresponds to the global scope and is created by the \c TopLevel AST
   * element.
   */
  private class Env(val variables: VarStack = mutable.ArrayStack()) {
    /**
     * Perform a deep copy of the current environment.
     */
    def copy(): Env = {
      val newVariables: VarStack = mutable.ArrayStack()
      variables.foreach(varMap => newVariables.push(varMap.clone))
      new Env(newVariables)
    }

    /**
     * Enter a new scope.
     *
     * This pushes an empty variable mapping into the environment.
     */
    def enterScope(ast: ASTNode): Unit =
      variables.push(mutable.Map())

    /**
     * Exit a scope.
     *
     * This destroys the top (i.e. most recent) variable mapping in the
     * environment.
     */
    def exitScope(ast: ASTNode): Unit =
      variables.pop()

    /**
     * Create a new, uninitialized variable in the environment.
     *
     * The variable is added to the top stack frame and initialized to ⊥.
     */
    def createVariable(name: Id): AbstractValue = {
      variables.head += name.text -> Bottom
      Bottom
    }

    /**
     * Create a new, initialized variable in the environment.
     *
     * The variable is added to the top stack frame and initialized with an
     * abstract representation of the given expression
     */
    def createVariable(name: Id, value: Expr): AbstractValue = {
      val absVal = makeAbstract(value)
      variables.head += name.text -> absVal
      absVal
    }

    /**
     * Update the abstract value of a variable in the environment.
     *
     * The variable must have already been defined in the environment. All
     * stack frames are searched from top to bottom to find the variable to
     * update.
     */
    def updateVariable(name: Id, value: Expr): AbstractValue = {
      val nameText = name.text
      variables.find(_.contains(nameText)) match {
        case Some(m) =>
          val absVal = makeAbstract(value)
          m += nameText -> absVal
          absVal
        // Technically the variable should have been defined before it can be
        // updated, but JavaScript is a funny language so we'll add the variable
        // to the current stack frame.
        case None => createVariable(name, value)
      }
    }

    /**
     * Retrieves a variable from the environment.
     *
     * Each scope is searched from "newest" to "oldest", where the oldest scope
     * is the global scope. If the variable isn't found \c None is returned.
     */
    def getVariable(name: Id): Option[AbstractValue] = {
      val nameText = name.text
      variables.find(_.contains(nameText)).flatMap(_.get(nameText))
    }

    /**
     * Get a list of non-constant variable declarations in the current (i.e.
     * top) stack frame. This is essentially a filter on variables that have
     * been set to ⊤.
     *
     * If the variable does not appear in the current stack frame, then set it
     * to ⊥ (which means it is uninitialized).
     */
    def filterConstantVarDecls(vds: List[VarDecl]): List[VarDecl] =
      vds.filter(vd => variables.head.getOrElse(vd.name.text, Bottom) == Top)

    /**
     * Performs a join (⊔) operation to find the least upper bounds between two
     * environments.
     *
     * This involves walking each stack frame in the environments and
     * performing a join on every variable in the stack frame.
     */
    def join(other: Env): Unit = {
      // v1 and v2 are VarMaps from the two environments and i is the current
      // index
      for ((v1, i) <- variables.zipWithIndex; v2 <- other.variables) {
        // Get the set of unique variable identifiers from the two VarMaps.
        // For each identifier retrieve its value from the two environments
        // (if the value doesn't exist we set it to ⊥, which means that it is
        // uninitialized).
        //
        // Perform the join operation on these two values, and create a
        // single variable identifier, value pair from this join operation.
        // We later transform these variable identifier, value pairs back
        // into a map.
        val varMap = (v1.keys ++ v2.keys).toSeq.distinct.map(
          id => id -> (v1.getOrElse(id, Bottom) join v2.getOrElse(id, Bottom))
        )
        // Convert the sequence of pairs back to a mutable map and update it in
        // the variable stack
        variables.update(i, mutable.Map(varMap: _*))
      }
    }
  }

  private object ConstantPropagationWalker extends ASTEnvWalker[Env] {
    /**
     * Expand a compound assignment expression.
     *
     * E.g. the following code:
     *
     * \code{.js}
     * a += 10;
     * \endcode
     *
     * Becomes:
     *
     * \code{.js}
     * a = a + 10;
     * \endcode
     *
     * This is useful because it allows us to simplify the program by
     * continually applying constant folding and constant propagation until we
     * reach a steady state.
     */
    private def expandCompoundAssignment(assign: AssignOpApp): AssignOpApp = assign match {
      // A compound assignment will always include the assignment operator
      // ("=") plus additional characters to the left of the assignment that
      // represent the binary operation to perform (e.g."+" for addition, "-"
      // for subtraction, etc.).
      //
      // Therefore, if there is only a single character in the assignment
      // operator, it must be a regular assignment and we do not need to expand
      // anything.
      case AssignOpApp(info, vr: VarRef, Op(opInfo, opText), right) if opText.length > 1 =>
        // Drop the assignment operator to give us the binary operation to
        // perform (e.g. "+", "<<", etc.)
        val binOp = opText.dropRight(1)
        // Generate a new binary expression for the right-hand side of the
        // assignment
        val newRight = InfixOpApp(right.info, vr, Op(opInfo, binOp), right)
        AssignOpApp(info, vr, Op(opInfo, "="), newRight)
      // Just a regular assignment
      case _ => assign
    }

    override def walk(node: TopLevel, env: Env): TopLevel = node match {
      case TopLevel(info, fds, vds, stmts) =>
        // Create the top level stack frame for all global variables and
        // functions
        env.enterScope(node)
        // Walk the global function declarations and perform constant
        // propagation in each function
        val newFds = fds.map(super.walk(_, env))
        // Walk the global variable declarations. This will create new variables
        // in the environment and set them to ⊥
        val newVds = vds.map(walk(_, env))
        // Perform constant propagation
        val newStmts = stmts.map(walk(_, env))
        // Filter out any variable declarations that have remained constant (or
        // ⊥) in this function - they are just dead code at this point
        // XXX don't do this for now
        val filteredVds = env.filterConstantVarDecls(newVds)
        // Get the list of variable names that remained constant in this function
        val constVarIds = newVds.filterNot(filteredVds.toSet).map(_.name)
        // Rewalk statements and remove all assignments to variables
        // that remained constant (these assignments are just dead code)
        val filteredStmts = newStmts.map(new RemoveConstantAssignmentWalker(constVarIds).walk(_))
        // We're finished - destroy this stack frame
        env.exitScope(node)
        TopLevel(info, newFds, filteredVds, filteredStmts)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: VarDecl, env: Env): VarDecl = node match {
      // Create a new variable in the environment.
      //
      // The variable should not have an initializer expression associated with
      // it - the AST rewriter's hoisting phase should have removed all of
      // these. If an initializer expression is found, throw an error.
      case VarDecl(info, name, expr, strict) => {
        expr match {
          case Some(_) => throw InitializedVariableError(name)
          case None =>
            env.createVariable(name)
            VarDecl(info, name, expr.map(walk(_, env)), strict)
        }
      }
    }

    override def walk(node: Functional, env: Env): Functional = node match {
      // Walking a function creates a new scope in the environment. Because the
      // AST rewriter's hoisting phase stores all variable declarations in vds,
      // we can just walk vds and save all of the variables into the
      // environment. The hoisting phase also ensures that the variable
      // declarations are uninitialized, so we set them all to ⊥.
      // 
      // Following this, the function body is walked. Once the function body
      // has been walked, we can exit that scope and return the new function.
      case Functional(info, fds, vds, SourceElements(seInfo, seBody, strict), name, params, body) =>
        // Create a new stack frame in the environment for the variables and
        // functions in this function
        env.enterScope(node)
        // Walk the local function declarations and perform constant propagation
        // in each function
        val newFds = fds.map(super.walk(_, env))
        // Walk the local variable declarations. This will create new variables
        // in the environment and set them to ⊥
        val newVds = vds.map(walk(_, env))
        // Perform constant propagation on the function body
        val newSeBody = seBody.map(walk(_, env))
        // Filter out any variable declarations that have remained constant (or
        // ⊥) in this function - they are just dead code at this point
        val filteredVds = env.filterConstantVarDecls(newVds)
        // Get the list of variable names that remained constant in this function
        val constVarIds = newVds.filterNot(filteredVds.toSet).map(_.name)
        // Rewalk the function body and remove all assignments to variables
        // that remained constant (these assignments are just dead code)
        val filteredSeBody = newSeBody.map(new RemoveConstantAssignmentWalker(constVarIds).walk(_))
        // We're finished - destroy this stack frame
        env.exitScope(node)
        Functional(info, newFds, filteredVds, SourceElements(seInfo, filteredSeBody, strict), name, params, body)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: Stmt, env: Env): Stmt = node match {
      // A code block creates a new scope in the environment. Once the block's
      // statements have been walked, we can exit that scope adn return the new
      // statements.
      case ABlock(info, stmts, internal) =>
        env.enterScope(node)
        val newStmts = stmts.map(walk(_, env))
        env.exitScope(node)
        ABlock(info, newStmts, internal)

      // If statements require us to analyse each branch individually. This
      // means that we must create a new copy of the environment when we walk
      // each branch of the if statement. After each branch has been walked, we
      // must merge all the environments together. This is known as a "join",
      // which is usually defined using the "⊔" operator.
      case If(info, cond, trueBranch, None) =>
        // Propagate constants into the conditional expression. Note that we do
        // not perform any dead code elimination if the conditional expression
        // becomes constant.
        val newCond = walk(cond, env)
        // Copy the current environment (because it contains mutable data
        // structures) and walk the true branch. This will modify the copied
        // environment and leave the original environment unchanged.
        val trueBranchEnv = env.copy
        val newTrueBranch = walk(trueBranch, trueBranchEnv)
        // Perform a join on the original environment and the environment
        // returned after walking the true branch
        env.join(trueBranchEnv)
        If(info, newCond, newTrueBranch, None)

      case If(info, cond, trueBranch, Some(falseBranch)) =>
        // Propagate constants into the conditional expression. Note that we do
        // not perform any dead code elimination if the conditional expression
        // becomes constant.
        val newCond = walk(cond, env)
        // Create copies of the environment to walk the true and false branches.
        val trueBranchEnv = env.copy
        val falseBranchEnv = env.copy
        // Walk the true and false branches with the copied environments. This
        // will modify the copied environments and leave the original
        // environment unchanged.
        val newTrueBranch = walk(trueBranch, trueBranchEnv)
        val newFalseBranch = walk(falseBranch, falseBranchEnv)
        // Perform a join on the true and false branch environments. Because we
        // want the results to propagate into the original environment passed
        // into this method, we perform a join twice on the original environment
        // and rely on the associativity of join (i.e.
        // (e1 ⊔ e2) ⊔ e3 === e1 ⊔ (e2 ⊔ e3))
        env.join(trueBranchEnv)
        env.join(falseBranchEnv)
        If(info, newCond, newTrueBranch, Some(newFalseBranch))

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: Expr, env: Env): Expr = node match {
      // First, expand any compound assignment expressions (e.g. "+=", "<<=",
      // etc.). If the expanded assignment expression is a regular assignment
      // to a variable, update that variable in the environment. Note that this
      // update will make non-constant expressions go to ⊤.
      case assign: AssignOpApp => expandCompoundAssignment(assign) match {
        case AssignOpApp(info, vr @ VarRef(_, id), op, right) =>
          val newRight = walk(right, env)
          env.updateVariable(id, newRight)
          AssignOpApp(info, vr, op, newRight)
        case _ => assign
      }

      // When a variable is used in an expression, check if it is a constant
      // expression in the abstract environment. If it is, just replace the
      // variable usage with its constant value. Otherwise leave unchanged.
      case vr @ VarRef(_, id) => env.getVariable(id) match {
        case Some(Constant(c)) => c
        case _ => vr
      }

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }
  }

  /**
   * Performs some form of dead code elimination - removing assignments to
   * variables that are deemed constant and have thus been propagated out of
   * existance.
   *
   * @param varIds Variable identifiers that have been marked as constant
   */
  private class RemoveConstantAssignmentWalker(varIds: List[Id]) extends ASTWalker {
    // If we are assignment a value to a variable that was deemed constant in
    // its scope, then we can safely remove all assignments to this variable.
    // Here "removal" means replace with an empty expression.
    override def walk(node: Expr): Expr = node match {
      case aoa @ AssignOpApp(info, vr @ VarRef(_, id), _, _) =>
        if (varIds.contains(id)) EmptyExpr(info) else aoa

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
