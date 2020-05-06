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
 * Renames variables and functions to something easily readable.
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
   * Mapping between the original identifiers and more "readable" identifiers.
   */
  private type IdMap = MutableMap[String, String]

  /**
   * A stack of identifier mappings.
   *
   * Each stack frame corresponds to a particular scope in the JavaScript
   * program.
   */
  private type IdStack = ArrayStack[IdMap]

  /**
   * The variable/function renamer environment.
   *
   * Tracks renamed variables and functions. When a renamed variable or
   * function is used, substitute its name.
   */
  private class Env(
      val varIds: IdStack = ArrayStack(),
      val funcIds: IdStack = ArrayStack()
  ) {
    private var animals: List[String] = DeobfuscatorUtil.animals
    private var animalCounter: Int = 0

    private var vegetables: List[String] = DeobfuscatorUtil.vegetables
    private var vegetableCounter: Int = 0

    /**
     * Enter a new scope.
     *
     * This pushes an empty identifier mapping into the environment.
     */
    def enterScope(ast: ASTNode): Unit = {
      varIds.push(MutableMap())
      funcIds.push(MutableMap())
    }

    /**
     * Exit a scope.
     *
     * This destroys the top (i.e. most recent) identifier mapping in the
     * environment.
     */
    def exitScope(ast: ASTNode): Unit = {
      varIds.pop()
      funcIds.pop()
    }

    /**
     * Generate a name based on a string and a counter.
     *
     * The counter is only used as a suffix if it we have done one full pass
     * through the animal/vegetable list.
     */
    private def genName(s: String, i: Int): String =
      s + (if (i > 0) s"_$i" else "")

    /**
     * Generate a string based on an animal name and some incrementing
     * counter (if all the animal names have been used at least once).
     */
    private def getAnimal(): String = {
      // Take the next animal from the list. If an animal doesn't exist,
      // reset the list
      val name = animals.headOption.getOrElse {
        animals = DeobfuscatorUtil.animals
        animalCounter += 1
        animals.head
      }

      // "Pop" the animal we just used from the list
      animals = animals.tail

      // Generate a name
      genName(name, animalCounter)
    }

    /**
     * Generate a string based on an animal name and some incrementing
     * counter (if all the animal names have been used at least once).
     */
    private def getVegetable(): String = {
      // Take the next vegetable from the list. If a vegetable doesn't exist,
      // reset the list
      val name = vegetables.headOption.getOrElse {
        vegetables = DeobfuscatorUtil.vegetables
        vegetableCounter += 1
        vegetables.head
      }

      // "Pop" the animal we just used from the list
      vegetables = vegetables.tail

      // Generate a name
      genName(name, vegetableCounter)
    }

    /**
     * Generate a new identifier for the given identifier and insert it into
     * the given ID stack.
     */
    private def genId(ids: IdStack, id: Id, newName: String): Id = id match {
      case Id(info, text, _, isWith) =>
        // Save the new variable name into the current stack frame
        ids.head += text -> newName

        // Return a new identifier
        Id(info, newName, Some(newName), isWith)
    }

    /**
     * Generate a new identifier for the given identifier and insert it into
     * the given ID stack.
     *
     * The old identifier is saved as a comment.
     */
    private def genIdWithComment(ids: IdStack, id: Id, newName: String): Id = id match {
      case Id(_, text, _, isWith) =>
        // Record the old variable name as a comment
        val newInfo = ASTNodeInfo(id.span, Some(Comment(id.info, text)))

        // Save the new variable name into the current stack frame
        varIds.head += text -> newName

        // Return a new identifier
        Id(newInfo, newName, Some(newName), isWith)
    }

    /**
     * Generate a new variable identifier (based on an animal name) for the
     * given identifier.
     */
    def genVarId(id: Id): Id =
      genId(varIds, id, getAnimal())

    /**
     * Generate a new function identifier (based on a vegetable name) for the
     * given identifier.
     *
     * Note that anonymous function expressions are possible. So if the name
     * is empty, keep it empty.
     */
    def genFuncId(id: Id): Id = id match {
      case Id(_, "", _, _) => id
      case _ => genId(funcIds, id, getVegetable())
    }

    /**
     * Generate a new variable identifier (based on an animal name) for the
     * given identifier.
     *
     * The old identifier is saved as a comment.
     */
    def genVarIdWithComment(id: Id): Id =
      genIdWithComment(varIds, id, getAnimal())

    /**
     * Retrieves a variable identifier from the environment.
     *
     * Each scope is searched from "newest" to "oldest", where the oldest scope
     * is the global scope. `None` is returned if the identifier isn't found.
     */
    def getVarId(id: Id): Option[Id] = id match {
      case Id(info, text, _, isWith) =>
        val makeId = (s: String) => Id(info, s, Some(s), isWith)
        varIds.find(_.contains(text)).flatMap(_.get(text)).map(makeId(_))
    }

    /**
     * Retrieves a function identifier from the environment.
     *
     * Each scope is searched from "newest" to "oldest", where the oldest scope
     * is the global scope. `None` is returned if the identifier isn't found.
     */
    def getFuncId(id: Id): Option[Id] = id match {
      case Id(info, text, _, isWith) =>
        val makeId = (s: String) => Id(info, s, Some(s), isWith)
        funcIds.find(_.contains(text)).flatMap(_.get(text)).map(makeId(_))
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
        // Rename all functions **before** walking the function bodies
        val renamedFds = fds.map {
          case FunDecl(info, Functional(ftnInfo, fds, vds, stmts, name, params, body), strict) =>
            val newName = env.genFuncId(name)
            val newFtn = Functional(ftnInfo, fds, vds, stmts, newName, params, body)
            FunDecl(info, newFtn, strict)
        }
        // Walk the global function declarations and rename variables within
        val newFds = renamedFds.map(walk(_, env))
        // Rename variable references in the top level code
        val newStmts = stmts.map(walk(_, env))
        // We're finished - destroy this stack frame
        env.exitScope(node)
        TopLevel(info, newFds, newVds, newStmts)

      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: Functional, env: Env): Functional = node match {
      case Functional(info, fds, vds, stmts, name, params, body) =>
        // Create a new stack frame for this function
        env.enterScope(node)
        // Rename the function parameters
        val newParams = params.map(env.genVarId(_))
        // Walk the local variable declarations. This will generate new
        // identifiers for these variables
        val newVds = vds.map(walk(_, env))
        // Rename all functions **before** walking the function bodies
        val renamedFds = fds.map {
          case FunDecl(info, Functional(ftnInfo, fds, vds, stmts, name, params, body), strict) =>
            val newName = env.genFuncId(name)
            val newFtn = Functional(ftnInfo, fds, vds, stmts, newName, params, body)
            FunDecl(info, newFtn, strict)
        }
        // Walk the local function declarations and rename variables within
        val newFds = renamedFds.map(walk(_, env))
        // Rename variable references in this function
        val newStmts = walk(stmts, env)
        // We're finished - destroy this stack frame
        env.exitScope(node)
        Functional(info, newFds, newVds, newStmts, name, newParams, body)

      case _ =>
        val newNode = super.walk(node, env)
        if (newNode != node) walk(newNode, env) else newNode
    }

    override def walk(node: VarDecl, env: Env): VarDecl = node match {
      // Rename variable declarations.
      case VarDecl(info, name, expr, isWith) =>
        val newName = env.genVarIdWithComment(name)
        VarDecl(info, newName, expr.map(walk(_, env)), isWith)
    }

    override def walk(node: LHS, env: Env): LHS = node match {
      // Rename the referenced variable.
      //
      // It is not necessary to declare a variable (with val) before using it.
      // Therefore if we don't find the variable in the environment, just leave
      // it
      case VarRef(info, id) => env.getVarId(id) match {
        case Some(newId) => VarRef(info, newId)
        case _ => super.walk(node, env)
      }

      // Rename the called function. The callee could be a function or a
      // function stored in a variable. The callee could also be an anonymous
      // function and may not be identified by a VarRef; in this case just
      // return the original FunApp with only the arguments updated
      case FunApp(info, fun, args) => {
        val newArgs = args.map(walk(_, env))
        fun match {
          case VarRef(vrInfo, id) =>
            val newId = env.getFuncId(id) orElse env.getVarId(id) getOrElse (id)
            FunApp(info, VarRef(vrInfo, newId), newArgs)
          case _ => FunApp(info, walk(fun, env), newArgs)
        }
      }

      case _ => super.walk(node, env)
    }
  }

  ////////////////////////////////////////////////////////////////
  // calculate results
  ////////////////////////////////////////////////////////////////

  (result, excLog)
}
