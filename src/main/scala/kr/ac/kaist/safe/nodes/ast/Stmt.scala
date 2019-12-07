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

// Program ::= Stmt*
case class Stmts(
    info: ASTNodeInfo,
    body: List[Stmt],
    strict: Boolean
) extends ASTNode {
  override def toString(indent: Int): String = ""

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Stmts(_, body1, strict1), Stmts(_, body2, strict2)) =>
      NU.fuzzyCompare(body1, body2) && strict1 == strict2
    case _ => false
  }
}

// Statements
trait Stmt extends ASTNode {
  def getIndent(indent: Int): Int = indent + 1
}

/**
 * Internally generated NoOperation
 * currently to denote the end of a file by Shell
 * Do not appear in the JavaScript source text
 */
case class NoOp(
    info: ASTNodeInfo,
    desc: String
) extends Stmt {
  override def toString(indent: Int): String = ""

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (NoOp(_, desc1), NoOp(_, desc2)) => desc1 == desc2
    case _ => false
  }
}

/**
 * Internally generated statement unit by Hoister
 * Do not appear in the JavaScript source text
 */
case class StmtUnit(
    info: ASTNodeInfo,
    stmts: List[Stmt]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("{")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1, stmts,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (StmtUnit(_, stmts1), StmtUnit(_, stmts2)) =>
      NU.fuzzyCompare(stmts1, stmts2)
    case _ => false
  }
}

// Stmt ::= function Id ( (Id,)* ) { Stmt* }
case class FunDecl(
    info: ASTNodeInfo,
    ftn: Functional,
    strict: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("function ")
      .append(ftn.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (FunDecl(_, ftn1, strict1), FunDecl(_, ftn2, strict2)) =>
      ftn1 =~ ftn2 && strict1 == strict2
    case _ => false
  }
}

// Stmt ::= { Stmt* }
case class ABlock(
    info: ASTNodeInfo,
    stmts: List[Stmt],
    internal: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("{")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        stmts,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }
  override def getIndent(indent: Int): Int = indent

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ABlock(_, stmts1, internal1), ABlock(_, stmts2, internal2)) =>
      NU.fuzzyCompare(stmts1, stmts2) && internal1 == internal2
    case _ => false
  }
}

// Stmt ::= var VarDecl(, VarDecl)* ;
case class VarStmt(
    info: ASTNodeInfo,
    vds: List[VarDecl]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    vds match {
      case Nil => s.toString
      case _ =>
        s.append("var ")
          .append(NU.join(indent, vds, ", ", new StringBuilder("")))
          .append(";")
        s.toString
    }
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (VarStmt(_, vds1), VarStmt(_, vds2)) =>
      NU.fuzzyCompare(vds1, vds2)
    case _ => false
  }
}

// Stmt ::= Id (= Expr)?
case class VarDecl(
    info: ASTNodeInfo,
    name: Id,
    expr: Option[Expr],
    strict: Boolean
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(name.toString(indent))
    expr.map(e => s.append(" = ").append(e.toString(indent)))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (VarDecl(_, name1, expr1, strict1), VarDecl(_, name2, expr2, strict2)) =>
      name1 =~ name2 && NU.fuzzyCompare(expr1, expr2) && strict1 == strict2
    case _ => false
  }
}

// Stmt ::= ;
case class EmptyStmt(
    info: ASTNodeInfo
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(";")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (EmptyStmt(_), EmptyStmt(_)) => true
    case _ => false
  }
}

// Stmt ::= Expr ;
case class ExprStmt(
    info: ASTNodeInfo,
    expr: Expr,
    internal: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(expr.toString(indent))
      .append(";")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ExprStmt(_, expr1, internal1), ExprStmt(_, expr2, internal2)) =>
      expr1 =~ expr2 && internal1 == internal2
    case _ => false
  }
}

// Stmt ::= if ( Expr ) Stmt (else Stmt)?
case class If(
    info: ASTNodeInfo,
    cond: Expr,
    trueBranch: Stmt,
    falseBranch: Option[Stmt]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val trueIndent = trueBranch.getIndent(indent)
    s.append("if (")
      .append(cond.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(trueIndent))
      .append(trueBranch.toString(trueIndent))
    falseBranch match {
      case Some(fb) =>
        val falseIndent = fb.getIndent(indent)
        s.append(LINE_SEP)
          .append(NU.getIndent(indent))
          .append("else")
          .append(LINE_SEP)
          .append(NU.getIndent(falseIndent))
          .append(fb.toString(falseIndent))
      case _ =>
    }
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (If(_, cond1, trueBranch1, falseBranch1), If(_, cond2, trueBranch2, falseBranch2)) =>
      cond1 =~ cond2 && trueBranch1 =~ trueBranch2 && NU.fuzzyCompare(falseBranch1, falseBranch2)
    case _ => false
  }
}

// Stmt ::= do Stmt while ( Expr ) ;
case class DoWhile(
    info: ASTNodeInfo,
    body: Stmt,
    cond: Expr
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    var bodyIndent = body.getIndent(indent)
    s.append("do")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
      .append("while (")
      .append(cond.toString(indent))
      .append(");")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (DoWhile(_, body1, cond1), DoWhile(_, body2, cond2)) =>
      body1 =~ body2 && cond1 =~ cond2
    case _ => false
  }
}

// Stmt ::= while ( Expr ) Stmt
case class While(
    info: ASTNodeInfo,
    cond: Expr,
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("while (")
    s.append(cond.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (While(_, cond1, body1), While(_, cond2, body2)) =>
      cond1 =~ cond2 && body1 =~ body2
    case _ => false
  }
}

// Stmt ::= for ( Expr? ; Expr? ; Expr? ) Stmt
case class For(
    info: ASTNodeInfo,
    init: Option[Expr],
    cond: Option[Expr],
    action: Option[Expr],
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for (")
    init.map(i => s.append(i.toString(indent)))
    s.append("; ")
    cond.map(c => s.append(c.toString(indent)))
    s.append("; ")
    action.map(a => s.append(a.toString(indent)))
    s.append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (For(_, init1, cond1, action1, body1), For(_, init2, cond2, action2, body2)) =>
      NU.fuzzyCompare(init1, init2) && NU.fuzzyCompare(cond1, cond2) &&
        NU.fuzzyCompare(action1, action2) && body1 =~ body2
    case _ => false
  }
}

// Stmt ::= for ( lhs in Expr ) Stmt
case class ForIn(
    info: ASTNodeInfo,
    lhs: LHS,
    expr: Expr,
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for (")
      .append(lhs.toString(indent))
      .append(" in ")
      .append(expr.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ForIn(_, lhs1, expr1, body1), ForIn(_, lhs2, expr2, body2)) =>
      lhs1 =~ lhs2 && expr1 =~ expr2 && body1 =~ body2
    case _ => false
  }
}

// Stmt ::= for ( var VarDecl(, VarDecl)* ; Expr? ; Expr? ) Stmt
case class ForVar(
    info: ASTNodeInfo,
    vars: List[VarDecl],
    cond: Option[Expr],
    action: Option[Expr],
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for (var ")
      .append(NU.join(
        indent,
        vars,
        ", ",
        new StringBuilder("")
      ))
      .append("; ")
    cond.map(c => s.append(c.toString(indent)))
    s.append("; ")
    action.map(a => s.append(a.toString(indent)))
    s.append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ForVar(_, vars1, cond1, action1, body1), ForVar(_, vars2, cond2, action2, body2)) =>
      NU.fuzzyCompare(vars1, vars2) && NU.fuzzyCompare(cond1, cond2) &&
        NU.fuzzyCompare(action1, action2) && body1 =~ body2
    case _ => false
  }
}

// Stmt ::= for ( var VarDecl in Expr ) Stmt
case class ForVarIn(
    info: ASTNodeInfo,
    vd: VarDecl,
    expr: Expr,
    body: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val bodyIndent = body.getIndent(indent)
    s.append("for (var ")
      .append(vd.toString(indent))
      .append(" in ")
      .append(expr.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(bodyIndent))
      .append(body.toString(bodyIndent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ForVarIn(_, vd1, expr1, body1), ForVarIn(_, vd2, expr2, body2)) =>
      vd1 =~ vd2 && expr1 =~ expr2 && body1 =~ body2
    case _ => false
  }
}

// Stmt ::= continue Label? ;
case class Continue(
    info: ASTNodeInfo,
    target: Option[Label]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("continue")
    target.map(t => s.append(" ").append(t.toString(indent)))
    s.append(";")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Continue(_, target1), Continue(_, target2)) =>
      NU.fuzzyCompare(target1, target2)
    case _ => false
  }
}

// Stmt ::= break Label? ;
case class Break(
    info: ASTNodeInfo,
    target: Option[Label]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("break")
    target.map(t => s.append(" ").append(t.toString(indent)))
    s.append(";")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Break(_, target1), Break(_, target2)) =>
      NU.fuzzyCompare(target1, target2)
    case _ => false
  }
}

// Stmt ::= return Expr? ;
case class Return(
    info: ASTNodeInfo,
    expr: Option[Expr]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("return")
    expr.map(e => s.append(" ").append(e.toString(indent)))
    s.append(";")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Return(_, expr1), Return(_, expr2)) =>
      NU.fuzzyCompare(expr1, expr2)
    case _ => false
  }
}

// Stmt ::= with ( Expr ) Stmt
case class With(
    info: ASTNodeInfo,
    expr: Expr,
    stmt: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val stmtIndent = stmt.getIndent(indent)
    s.append("with (")
      .append(expr.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(stmtIndent))
      .append(stmt.toString(stmtIndent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (With(_, expr1, stmt1), With(_, expr2, stmt2)) =>
      expr1 =~ expr2 && stmt1 =~ stmt2
    case _ => false
  }
}

// Stmt ::= switch ( Expr ) { CaseClause* (default : Stmt*)? CaseClause* }
case class Switch(
    info: ASTNodeInfo,
    cond: Expr,
    frontCases: List[Case],
    defopt: Option[List[Stmt]],
    backCases: List[Case]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("switch (")
      .append(cond.toString(indent))
      .append(") {")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        frontCases,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
    defopt.map(d => {
      s.append(LINE_SEP)
        .append(NU.getIndent(indent + 1))
        .append("default:")
        .append(LINE_SEP)
        .append(NU.getIndent(indent + 2))
        .append(NU.join(
          indent + 2,
          d,
          LINE_SEP + NU.getIndent(indent + 2),
          new StringBuilder("")
        ))
    })
    s.append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        backCases,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Switch(_, cond1, frontCases1, defopt1, backCases1), Switch(_, cond2, frontCases2, defopt2, backCases2)) =>
      cond1 =~ cond2 && NU.fuzzyCompare(frontCases1, frontCases2) &&
        NU.fuzzyCompare(defopt1.getOrElse(Nil), defopt2.getOrElse(Nil)) && NU.fuzzyCompare(backCases1, backCases2)
    case _ => false
  }
}

// CaseClause ::= case Expr : Stmt*
case class Case(
    info: ASTNodeInfo,
    cond: Expr,
    body: List[Stmt]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("case ")
      .append(cond.toString(indent))
      .append(":")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Case(_, cond1, body1), Case(_, cond2, body2)) =>
      cond1 =~ cond2 && NU.fuzzyCompare(body1, body2)
    case _ => false
  }
}

// Stmt ::= Label : Stmt
case class LabelStmt(
    info: ASTNodeInfo,
    label: Label,
    stmt: Stmt
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(label.toString(indent))
      .append(" : ")
      .append(stmt.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (LabelStmt(_, label1, stmt1), LabelStmt(_, label2, stmt2)) =>
      label1 =~ label2 && stmt1 =~ stmt2
    case _ => false
  }
}

// Stmt ::= throw Expr ;
case class Throw(
    info: ASTNodeInfo,
    expr: Expr
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("throw ")
      .append(expr.toString(indent))
      .append(";")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Throw(_, expr1), Throw(_, expr2)) => expr1 =~ expr2
    case _ => false
  }
}

// Stmt ::= try { Stmt* } (catch ( Id ) { Stmt* })? (finally { Stmt* })?
case class Try(
    info: ASTNodeInfo,
    body: List[Stmt],
    catchBlock: Option[Catch],
    fin: Option[List[Stmt]]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("try {")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    catchBlock.map(c => {
      s.append(LINE_SEP)
        .append(NU.getIndent(indent))
        .append(c.toString(indent))
    })
    fin.map(f => {
      s.append(LINE_SEP)
        .append(NU.getIndent(indent))
        .append("finally {")
        .append(LINE_SEP)
        .append(NU.getIndent(indent + 1))
        .append(NU.join(
          indent + 1,
          f,
          LINE_SEP + NU.getIndent(indent + 1),
          new StringBuilder("")
        ))
        .append(LINE_SEP)
        .append(NU.getIndent(indent))
        .append("}")
    })
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Try(_, body1, catchBlock1, fin1), Try(_, body2, catchBlock2, fin2)) =>
      NU.fuzzyCompare(body1, body2) && NU.fuzzyCompare(catchBlock1, catchBlock2) &&
        NU.fuzzyCompare(fin1.getOrElse(Nil), fin2.getOrElse(Nil))
    case _ => false
  }
}

// Catch ::= catch ( Id ) { Stmt* }
case class Catch(
    info: ASTNodeInfo,
    id: Id,
    body: List[Stmt]
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("catch (")
      .append(id.toString(indent))
      .append(") {")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        body,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Catch(_, id1, body1), Catch(_, id2, body2)) =>
      id1 =~ id2 && NU.fuzzyCompare(body1, body2)
    case _ => false
  }
}

// Stmt ::= debugger ;
case class Debugger(
    info: ASTNodeInfo
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("debugger;")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Debugger(_), Debugger(_)) => true
    case _ => false
  }
}

// Stmt ::= function Id (.Id)+ ( (Id,)* ) { SourceElement* }
case class JScriptMemFunDecl(
    override val info: ASTNodeInfo,
    obj: Id,
    members: List[Id],
    ftn: Functional,
    strict: Boolean
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("function ")
      .append(obj.toString(indent))
      .append(".")
      .append(NU.join(indent, members, ".", new StringBuilder("")))
    if (!members.isEmpty) s.append(".")
    s.append(ftn.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (JScriptMemFunDecl(_, obj1, members1, ftn1, strict1), JScriptMemFunDecl(_, obj2, members2, ftn2, strict2)) =>
      obj1 =~ obj2 && NU.fuzzyCompare(members1, members2) && ftn1 =~ ftn2 &&
        strict1 == strict2
    case _ => false
  }
}

// Stmt ::= /*@cc_on Stmt* @*/
case class JScriptConditionalCompilation(
    override val info: ASTNodeInfo,
    stmts: List[Stmt]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    s.append("/*@cc_on")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        stmts,
        LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("@*/")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (JScriptConditionalCompilation(_, stmts1), JScriptConditionalCompilation(_, stmts2)) =>
      NU.fuzzyCompare(stmts1, stmts2)
    case _ => false
  }
}

// Stmt ::= @if ( Expr ) Stmt (@elif ( Expr ) Stmt)* (@else Stmt)? @endif
case class JScriptConditionalIf(
    override val info: ASTNodeInfo,
    conds: List[Expr],
    trueBranches: List[Stmt],
    falseBranch: Option[Stmt]
) extends Stmt {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    val trueIndent = trueBranches.head.getIndent(indent)
    val elifToString = (cond: Expr, branch: Stmt) => {
      val branchIndent = branch.getIndent(indent)
      val s: StringBuilder = new StringBuilder
      s.append(LINE_SEP)
      s.append(NU.getIndent(indent))
        .append("@elif (")
        .append(cond.toString(indent))
        .append(")")
        .append(LINE_SEP)
        .append(NU.getIndent(branchIndent))
        .append(branch.toString(branchIndent))
    }
    s.append("@if (")
      .append(conds.head.toString(indent))
      .append(")")
      .append(LINE_SEP)
      .append(NU.getIndent(trueIndent))
      .append(trueBranches.head.toString(trueIndent))
      .append((conds.tail zip trueBranches.tail).map { case (c, b) => elifToString(c, b) }.mkString)
    falseBranch match {
      case Some(fb) =>
        val falseIndent = fb.getIndent(indent)
        s.append(LINE_SEP)
          .append(NU.getIndent(indent))
          .append("@else")
          .append(LINE_SEP)
          .append(NU.getIndent(falseIndent))
          .append(fb.toString(falseIndent))
      case _ =>
    }
    s.append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("@end")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (JScriptConditionalIf(_, conds1, trueBranches1, falseBranch1), JScriptConditionalIf(_, conds2, trueBranches2, falseBranch2)) =>
      NU.fuzzyCompare(conds1, conds2) && NU.fuzzyCompare(trueBranches1, trueBranches2) && NU.fuzzyCompare(falseBranch1, falseBranch2)
    case _ => false
  }
}
