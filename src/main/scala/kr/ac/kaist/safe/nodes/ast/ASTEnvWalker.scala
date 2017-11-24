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

package kr.ac.kaist.safe.nodes.ast

trait ASTEnvWalker[Env] {
  def walk(info: ASTNodeInfo, env: Env): ASTNodeInfo = info match {
    case ASTNodeInfo(span, comment) =>
      ASTNodeInfo(span, comment.map(walk(_, env)))
  }

  def walk(node: ASTNode, env: Env): ASTNode = node match {
    case p: Program => walk(p, env)
    case s: SourceElement => walk(s, env)
    case s: SourceElements => walk(s, env)
    case v: VarDecl => walk(v, env)
    case c: Case => walk(c, env)
    case c: Catch => walk(c, env)
    case e: Expr => walk(e, env)
    case p: Property => walk(p, env)
    case m: Member => walk(m, env)
    case i: Id => walk(i, env)
    case o: Op => walk(o, env)
    case a: AnonymousFnName => walk(a, env)
    case l: Label => walk(l, env)
    case c: Comment => walk(c, env)
    case t: TopLevel => walk(t, env)
    case f: Functional => walk(f, env)
  }

  def walk(node: Program, env: Env): Program = node match {
    case Program(info, body) =>
      Program(walk(info, env), walk(body, env))
  }

  def walk(node: Stmt, env: Env): Stmt = node match {
    case NoOp(info, desc) =>
      NoOp(walk(info, env), desc)
    case StmtUnit(info, stmts) =>
      StmtUnit(walk(info, env), stmts.map(walk(_, env)))
    case fd: FunDecl =>
      walk(fd, env)
    case ABlock(info, stmts, isInternal) =>
      ABlock(walk(info, env), stmts.map(walk(_, env)), isInternal)
    case VarStmt(info, vds) =>
      VarStmt(walk(info, env), vds.map(walk(_, env)))
    case EmptyStmt(info) =>
      EmptyStmt(walk(info, env))
    case ExprStmt(info, expr, isInternal) =>
      ExprStmt(walk(info, env), walk(expr, env), isInternal)
    case If(info, cond, trueB, falseB) =>
      If(walk(info, env), walk(cond, env), walk(trueB, env), falseB.map(walk(_, env)))
    case DoWhile(info, body, cond) =>
      DoWhile(walk(info, env), walk(body, env), walk(cond, env))
    case While(info, cond, body) =>
      While(walk(info, env), walk(cond, env), walk(body, env))
    case For(info, init, cond, action, body) =>
      For(walk(info, env), init.map(walk(_, env)), cond.map(walk(_, env)), action.map(walk(_, env)), walk(body, env))
    case ForIn(info, lhs, expr, body) =>
      ForIn(walk(info, env), walk(lhs, env), walk(expr, env), walk(body, env))
    case ForVar(info, vars, cond, action, body) =>
      ForVar(walk(info, env), vars.map(walk(_, env)), cond.map(walk(_, env)), action.map(walk(_, env)), walk(body, env))
    case ForVarIn(info, vari, expr, body) =>
      ForVarIn(walk(info, env), walk(vari, env), walk(expr, env), walk(body, env))
    case Continue(info, target) =>
      Continue(walk(info, env), target.map(walk(_, env)))
    case Break(info, target) =>
      Break(walk(info, env), target.map(walk(_, env)))
    case Return(info, expr) =>
      Return(walk(info, env), expr.map(walk(_, env)))
    case With(info, expr, stmt) =>
      With(walk(info, env), walk(expr, env), walk(stmt, env))
    case Switch(info, cond, frontCases, defi, backCases) =>
      Switch(walk(info, env), walk(cond, env), frontCases.map(walk(_, env)), defi.map(_.map(walk(_, env))), backCases.map(walk(_, env)))
    case LabelStmt(info, label, stmt) =>
      LabelStmt(walk(info, env), walk(label, env), walk(stmt, env))
    case Throw(info, expr) =>
      Throw(walk(info, env), walk(expr, env))
    case Try(info, body, catchBlock, fin) =>
      Try(walk(info, env), body.map(walk(_, env)), catchBlock.map(walk(_, env)), fin.map(_.map(walk(_, env))))
    case Debugger(info) =>
      Debugger(walk(info, env))
    case JScriptMemFunDecl(info, obj, members, ftn, isStrict) =>
      JScriptMemFunDecl(walk(info, env), walk(obj, env), members.map(walk(_, env)), walk(ftn, env), isStrict)
    case JScriptConditionalCompilation(info, stmts) =>
      JScriptConditionalCompilation(walk(info, env), stmts.map(walk(_, env)))
    case JScriptConditionalIf(info, conds, trueBranches, falseBranch) =>
      JScriptConditionalIf(walk(info, env), conds.map(walk(_, env)), trueBranches.map(walk(_, env)), falseBranch.map(walk(_, env)))
  }

  def walk(node: Expr, env: Env): Expr = node match {
    case ExprList(info, exprs) =>
      ExprList(walk(info, env), exprs.map(walk(_, env)))
    case Cond(info, cond, trueB, falseB) =>
      Cond(walk(info, env), walk(cond, env), walk(trueB, env), walk(falseB, env))
    case InfixOpApp(info, left, op, right) =>
      InfixOpApp(walk(info, env), walk(left, env), walk(op, env), walk(right, env))
    case PrefixOpApp(info, op, right) =>
      PrefixOpApp(walk(info, env), walk(op, env), walk(right, env))
    case UnaryAssignOpApp(info, lhs, op) =>
      UnaryAssignOpApp(walk(info, env), walk(lhs, env), walk(op, env))
    case AssignOpApp(info, lhs, op, right) =>
      AssignOpApp(walk(info, env), walk(lhs, env), walk(op, env), walk(right, env))
    case l: LHS =>
      walk(l, env)
    case EmptyExpr(info) =>
      EmptyExpr(walk(info, env))
  }

  def walk(node: LHS, env: Env): LHS = node match {
    case This(info) =>
      This(walk(info, env))
    case Null(info) =>
      Null(walk(info, env))
    case Undefined(info) =>
      Undefined(walk(info, env))
    case Bool(info, isBool) =>
      Bool(walk(info, env), isBool)
    case n: NumberLiteral =>
      walk(n, env)
    case StringLiteral(info, quote, escaped, isRE) =>
      StringLiteral(walk(info, env), quote, escaped, isRE)
    case RegularExpression(info, body, flag) =>
      RegularExpression(walk(info, env), body, flag)
    case VarRef(info, id) =>
      VarRef(walk(info, env), walk(id, env))
    case ArrayExpr(info, elements) =>
      ArrayExpr(walk(info, env), elements.map(_.map(walk(_, env))))
    case ArrayNumberExpr(info, elements) =>
      ArrayNumberExpr(walk(info, env), elements)
    case ObjectExpr(info, members) =>
      ObjectExpr(walk(info, env), members.map(walk(_, env)))
    case Parenthesized(info, expr) =>
      Parenthesized(walk(info, env), walk(expr, env))
    case FunExpr(info, ftn) =>
      FunExpr(walk(info, env), walk(ftn, env))
    case Bracket(info, obj, index) =>
      Bracket(walk(info, env), walk(obj, env), walk(index, env))
    case Dot(info, obj, member) =>
      Dot(walk(info, env), walk(obj, env), walk(member, env))
    case New(info, lhs) =>
      New(walk(info, env), walk(lhs, env))
    case FunApp(info, fun, args) =>
      FunApp(walk(info, env), walk(fun, env), args.map(walk(_, env)))
    case JScriptMemFunExpr(info, obj, members, ftn) =>
      JScriptMemFunExpr(walk(info, env), walk(obj, env), members.map(walk(_, env)), walk(ftn, env))
  }

  def walk(node: NumberLiteral, env: Env): NumberLiteral = node match {
    case DoubleLiteral(info, text, num) =>
      DoubleLiteral(walk(info, env), text, num)
    case IntLiteral(info, intVal, radix) =>
      IntLiteral(walk(info, env), intVal, radix)
  }

  def walk(node: SourceElement, env: Env): SourceElement = node match {
    case s: Stmt =>
      walk(s, env)
  }

  def walk(node: SourceElements, env: Env): SourceElements = node match {
    case SourceElements(info, body, isStrict) =>
      SourceElements(walk(info, env), body.map(walk(_, env)), isStrict)
  }

  def walk(node: FunDecl, env: Env): FunDecl = node match {
    case FunDecl(info, ftn, isStrict) =>
      FunDecl(walk(info, env), walk(ftn, env), isStrict)
  }

  def walk(node: VarDecl, env: Env): VarDecl = node match {
    case VarDecl(info, name, expr, isStrict) =>
      VarDecl(walk(info, env), walk(name, env), expr.map(walk(_, env)), isStrict)
  }

  def walk(node: Case, env: Env): Case = node match {
    case Case(info, cond, body) =>
      Case(walk(info, env), walk(cond, env), body.map(walk(_, env)))
  }

  def walk(node: Catch, env: Env): Catch = node match {
    case Catch(info, id, body) =>
      Catch(walk(info, env), walk(id, env), body.map(walk(_, env)))
  }

  def walk(node: Property, env: Env): Property = node match {
    case PropId(info, id) =>
      PropId(walk(info, env), walk(id, env))
    case PropStr(info, str) =>
      PropStr(walk(info, env), str)
    case PropNum(info, num) =>
      PropNum(walk(info, env), walk(num, env))
  }

  def walk(node: Member, env: Env): Member = node match {
    case Field(info, prop, expr) =>
      Field(walk(info, env), walk(prop, env), walk(expr, env))
    case GetProp(info, prop, ftn) =>
      GetProp(walk(info, env), walk(prop, env), walk(ftn, env))
    case SetProp(info, prop, ftn) =>
      SetProp(walk(info, env), walk(prop, env), walk(ftn, env))
  }

  def walk(node: Id, env: Env): Id = node match {
    case Id(info, text, uniqueName, isWith) =>
      Id(walk(info, env), text, uniqueName, isWith)
  }

  def walk(node: Op, env: Env): Op = node match {
    case Op(info, text) =>
      Op(walk(info, env), text)
  }

  def walk(node: AnonymousFnName, env: Env): AnonymousFnName = node match {
    case AnonymousFnName(info, text) =>
      AnonymousFnName(walk(info, env), text)
  }

  def walk(node: Label, env: Env): Label = node match {
    case Label(info, id) =>
      Label(walk(info, env), walk(id, env))
  }

  def walk(node: Comment, env: Env): Comment = node match {
    case Comment(info, comment) =>
      Comment(walk(info, env), comment)
  }

  def walk(node: TopLevel, env: Env): TopLevel = node match {
    case TopLevel(info, fds, vds, stmts) =>
      TopLevel(walk(info, env), fds.map(walk(_, env)), vds.map(walk(_, env)), stmts.map(walk(_, env)))
  }

  def walk(node: Functional, env: Env): Functional = node match {
    case Functional(info, fds, vds, stmts, name, params, body) =>
      Functional(walk(info, env), fds.map(walk(_, env)), vds.map(walk(_, env)), walk(stmts, env), walk(name, env),
        params.map(walk(_, env)), body)
  }
}
