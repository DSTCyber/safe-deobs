/**
 * *****************************************************************************
 * Copyright (c) 2016-2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.nodes.ast

import kr.ac.kaist.safe.util.{ NodeUtil => NU }
import kr.ac.kaist.safe.LINE_SEP

// Common body for program and functions
trait ScopeBody extends ASTNode {
  val fds: List[FunDecl]
  val vds: List[VarDecl]
}

// Program top level
case class TopLevel(
    info: ASTNodeInfo,
    fds: List[FunDecl],
    vds: List[VarDecl],
    stmts: List[Stmts]
) extends ScopeBody {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    NU.prUseStrictDirective(s, indent, fds, vds, stmts)
    NU.prFtn(s, indent, fds, vds,
      stmts.foldLeft(List[Stmt]()) {
        case (l, s) => l ++ s.body.asInstanceOf[List[Stmt]]
      })
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (TopLevel(_, fds1, vds1, stmts1), TopLevel(_, fds2, vds2, stmts2)) =>
      NU.fuzzyCompare(fds1, fds2) && NU.fuzzyCompare(vds1, vds2) && NU.fuzzyCompare(stmts1, stmts2)
    case _ => false
  }
}

// Common shape for functions
case class Functional(
    info: ASTNodeInfo,
    fds: List[FunDecl],
    vds: List[VarDecl],
    stmts: Stmts,
    name: Id,
    params: List[Id],
    body: String
) extends ScopeBody {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append(name.toString(indent))
      .append("(")
      .append(NU.join(indent, params, ", ", new StringBuilder("")))
      .append(") {")
      .append(LINE_SEP)
    NU.prUseStrictDirective(s, indent, fds, vds, stmts)
    NU.prFtn(s, indent, fds, vds, stmts.body)
    s.append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Functional(_, fds1, vds1, stmts1, name1, params1, body1), Functional(_, fds2, vds2, stmts2, name2, params2, body2)) =>
      NU.fuzzyCompare(fds1, fds2) && NU.fuzzyCompare(vds1, vds2) && (stmts1 =~ stmts2) && (name1 =~ name2) &&
        NU.fuzzyCompare(params1, params2) && (body1 == body2)
    case _ => false
  }
}
