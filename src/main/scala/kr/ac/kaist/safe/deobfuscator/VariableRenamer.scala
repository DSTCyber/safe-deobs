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

import scala.collection.mutable.{ ArrayStack, Map => MutableMap }

import kr.ac.kaist.safe.errors.ExcLog
import kr.ac.kaist.safe.nodes.ast._
import kr.ac.kaist.safe.util.DeobfuscatorUtil

/**
 * Renames variables to something easily readable.
 */
class VariableRenamer(program: Program) {
  ////////////////////////////////////////////////////////////////
  // results
  ////////////////////////////////////////////////////////////////

  lazy val result: Program = VariableRenameWalker.walk(program, new Env)

  lazy val excLog: ExcLog = new ExcLog

  ////////////////////////////////////////////////////////////////
  // private global
  ////////////////////////////////////////////////////////////////

  /**
   * Mapping between original variable identifiers and more "readable" variable
   * identifiers.
   */
  private type IdMap = MutableMap[String, String]

  /**
   * A stack of identifier mappings.
   *
   * Each stack frame corresponds to a particular scope in the JavaScript
   * program.
   */
  private type IdStack = ArrayStack[IdMap]

  private class Env(val ids: IdStack = ArrayStack()) {
    private var suffixCounter = 0;

    private var animals = DeobfuscatorUtil.animals

    /**
     * Enter a new scope.
     *
     * This pushes an empty identifier mapping into the environment.
     */
    def enterScope(ast: ASTNode): Unit =
      ids.push(MutableMap())

    /**
     * Exit a scope.
     *
     * This destroys the top (i.e. most recent) identifier mapping in the
     * environment.
     */
    def exitScope(ast: ASTNode): Unit =
      ids.pop()

    /**
     * Generate a new identifier (based on an animal name) for the given
     * identifier.
     */
    def genId(id: Id): Id = id match {
      case Id(info, text, _, isWith) =>
        // Take the next animal from the list. If an animal doesn't exist,
        // reset the list.
        val newName = animals.headOption.getOrElse {
          animals = DeobfuscatorUtil.animals
          suffixCounter += 1
          animals.head
        }
        // Only add a suffix once we've done one full iteration over the animal
        // list
        val suffix = if (suffixCounter > 0) s"_$suffixCounter" else s""
        val newText = newName + suffix

        // "Pop" the animal we just used from the list
        animals = animals.tail
        // Save the new variable name into the current stack frame
        ids.head += text -> newText

        // Return a new identifier
        Id(info, newText, Some(newText), isWith)
    }

    /**
     * Retrieves a variable name from the environment.
     *
     * Each scope is searched from "newest" to "oldest", where the oldest scope
     * is the global scope. `None` is returned if the identifier isn't found.
     */
    def getName(id: Id): Option[String] = {
      val idText = id.text
      ids.find(_.contains(idText)).flatMap(_.get(idText))
    }
  }

  private object VariableRenameWalker extends ASTEnvWalker[Env] {
    override def walk(node: TopLevel, env: Env): TopLevel = node match {
      case TopLevel(info, fds, vds, stmts) =>
        // Create the top level stack frame for all global variables and
        // functions
        env.enterScope(node)
        // Walk the global variable declarations. This will generate new
        // identifiers for these variables
        val newVds = vds.map(walk(_, env))
        // Walk the global function declarations and rename variables
        // within
        val newFds = fds.map(walk(_, env))
        // Rename variable references in the top level code
        val newStmts = stmts.map(walk(_, env))
        // We're finished - destroy this stack frame
        env.exitScope(node)
        TopLevel(info, newFds, newVds, newStmts)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: Functional, env: Env): Functional = node match {
      case Functional(info, fds, vds, stmts, name, params, body) =>
        // Create a new stack frame for this function
        env.enterScope(node)
        // Walk the local variable declarations. This will generate new
        // identifiers for these variables
        val newVds = vds.map(walk(_, env))
        // Walk the local function declarations and rename variables within
        val newFds = fds.map(walk(_, env))
        // Rename variable references in this function
        val newStmts = walk(stmts, env)
        // We're finished - destroy this stack frame
        env.exitScope(node)
        Functional(info, newFds, newVds, newStmts, name, params, body)

      // Rewalk the node if a change has been made to the AST
      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: VarDecl, env: Env): VarDecl = node match {
      // Rename variable declarations.
      case VarDecl(info, name, expr, isWith) =>
        val newName = env.genId(name)
        VarDecl(info, newName, expr.map(walk(_, env)), isWith)
    }

    override def walk(node: LHS, env: Env): LHS = node match {
      // Rename the referenced variable.
      //
      // It is not necessary to declare a variable (with val) before using it.
      // Therefore if we don't find the variable in the environment, just leave
      // it
      case VarRef(info, id @ Id(idInfo, _, _, isWith)) => env.getName(id) match {
        case Some(newId) => VarRef(info, Id(idInfo, newId, Some(newId), isWith))
        case _ => node
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
