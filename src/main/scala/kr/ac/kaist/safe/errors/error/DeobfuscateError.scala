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

package kr.ac.kaist.safe.errors.error

import kr.ac.kaist.safe.nodes.ast._

sealed abstract class DeobfuscateError(msg: String, ast: ASTNode) extends SafeError({
  s"${ast.info.span}: $msg"
})

////////////////////////////////////////////////////////////////
// constant propagation errors
////////////////////////////////////////////////////////////////

sealed abstract class ConstantPropagationError(msg: String, ast: ASTNode) extends DeobfuscateError(msg, ast)

case class InitializedVariableError(id: Id) extends ConstantPropagationError({
  s"The variable '${id.text}' should not have an initializer - the AST writer should have removed this."
}, id)

case class UnsupportedStatementError(stmt: Stmt) extends ConstantPropagationError({
  s"Constant propagation through ${stmt.getClass.getSimpleName} statements is not yet supported."
}, stmt)