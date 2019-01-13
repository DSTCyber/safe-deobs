/**
 * *****************************************************************************
 * Copyright (c) 2017, KAIST.
 * Copyright (c) 2019, DST Group.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.json

import kr.ac.kaist.safe.nodes.ast._

import spray.json._
import DefaultJsonProtocol._

/**
 * Render AST in JSON format compatible with
 * https://bl.ocks.org/d3noob/80c100e35817395e88918627eeeac717#index.html
 */
object ASTD3Protocol extends DefaultJsonProtocol {

  implicit object ASTNodeD3JsonFormat extends RootJsonFormat[ASTNode] {
    def toJson(node: Program): JsValue = node match {
      case Program(_, body) => JsObject(
        "name" -> JsString("program"),
        "children" -> JsArray(toJson(body))
      )
    }

    def toJson(node: Stmt): JsValue = node match {
      case _: NoOp => JsNull
      case StmtUnit(_, stmts) => JsObject(
        "name" -> JsString("statements"),
        "children" -> JsArray(stmts.map(toJson).to[Vector])
      )
      case fd: FunDecl => toJson(fd)
      case ABlock(_, stmts, _) => JsObject(
        "name" -> JsString("stmt block"),
        "children" -> JsArray(stmts.map(toJson).to[Vector])
      )
      case VarStmt(_, vds) => JsObject(
        "name" -> JsString("variable"),
        "children" -> JsArray(vds.map(toJson).to[Vector])
      )
      case EmptyStmt(_) => JsObject("name" -> JsString("empty"))
      case ExprStmt(_, expr, _) => JsObject(
        "name" -> JsString("expression"),
        "children" -> JsArray(toJson(expr))
      )
      case If(_, cond, trueB, falseB) => JsObject(
        "name" -> JsString("if"),
        "children" -> JsArray(toJson(cond), toJson(trueB),
          falseB.map(toJson).getOrElse(JsNull))
      )
      case DoWhile(_, body, cond) => JsObject(
        "name" -> JsString("do while"),
        "children" -> JsArray(toJson(body), toJson(cond))
      )
      case While(_, cond, body) => JsObject(
        "name" -> JsString("while"),
        "children" -> JsArray(toJson(cond), toJson(body))
      )
      case For(_, init, cond, action, body) => JsObject(
        "name" -> JsString("for"),
        "children" -> JsArray(
          init.map(toJson).getOrElse(JsNull),
          cond.map(toJson).getOrElse(JsNull),
          action.map(toJson).getOrElse(JsNull), toJson(body)
        )
      )
      case ForIn(_, lhs, expr, body) => JsObject(
        "name" -> JsString("for in"),
        "children" -> JsArray(toJson(lhs), toJson(expr), toJson(body))
      )
      case ForVar(_, vars, cond, action, body) => JsObject(
        "name" -> JsString("for var"),
        "children" -> JsArray(
          vars.map(toJson).to[Vector] :+
            cond.map(toJson).getOrElse(JsNull) :+
            action.map(toJson).getOrElse(JsNull) :+ toJson(body)
        )
      )
      case ForVarIn(_, vari, expr, body) => JsObject(
        "name" -> JsString("for var in"),
        "children" -> JsArray(toJson(vari), toJson(expr), toJson(body))
      )
      case Continue(_, target) => JsObject(
        "name" -> JsString("continue"),
        "children" -> JsArray(target.map(toJson).getOrElse(JsNull))
      )
      case Break(_, target) => JsObject(
        "name" -> JsString("break"),
        "children" -> JsArray(target.map(toJson).getOrElse(JsNull))
      )
      case Return(_, expr) => JsObject(
        "name" -> JsString("return"),
        "children" -> JsArray(expr.map(toJson).getOrElse(JsNull))
      )
      case With(_, expr, stmt) => JsObject(
        "name" -> JsString("with"),
        "children" -> JsArray(toJson(expr), toJson(stmt))
      )
      case Switch(_, cond, frontCases, defi, backCases) => JsObject(
        "name" -> JsString("switch"),
        "children" -> JsArray((toJson(cond) +:
          frontCases.map(toJson).to[Vector]) ++
          defi.map(_.map(toJson)).getOrElse(List(JsNull)) ++
          backCases.map(toJson))
      )
      case LabelStmt(_, label, stmt) => JsObject(
        "name" -> JsString("label"),
        "children" -> JsArray(toJson(label), toJson(stmt))
      )
      case Throw(_, expr) => JsObject(
        "name" -> JsString("throw"),
        "children" -> JsArray(toJson(expr))
      )
      case Try(_, body, catchBlock, fin) => JsObject(
        "name" -> JsString("try"),
        "children" -> JsArray((body.map(toJson).to[Vector] :+
          catchBlock.map(toJson).getOrElse(JsNull)) ++
          fin.map(_.map(toJson)).getOrElse(List(JsNull)))
      )
      case Debugger(_) => JsObject("name" -> JsString("debugger"))
    }

    def toJson(node: Expr): JsValue = node match {
      case ExprList(_, exprs) => JsObject(
        "name" -> JsString("expressions"),
        "children" -> JsArray(exprs.map(toJson).to[Vector])
      )
      case Cond(_, cond, trueB, falseB) => JsObject(
        "name" -> JsString("cond"),
        "children" -> JsArray(toJson(cond), toJson(trueB), toJson(falseB))
      )
      case InfixOpApp(_, left, op, right) => JsObject(
        "name" -> JsString("infix op"),
        "children" -> JsArray(toJson(left), toJson(op), toJson(right))
      )
      case PrefixOpApp(_, op, right) => JsObject(
        "name" -> JsString("prefix op"),
        "children" -> JsArray(toJson(op), toJson(right))
      )
      case UnaryAssignOpApp(_, lhs, op) => JsObject(
        "name" -> JsString("unary assign op"),
        "children" -> JsArray(toJson(lhs), toJson(op))
      )
      case AssignOpApp(_, lhs, op, right) => JsObject(
        "name" -> JsString("assign op"),
        "children" -> JsArray(toJson(lhs), toJson(op), toJson(right))
      )
      case l: LHS => toJson(l)
      case EmptyExpr(_) => JsObject("name" -> JsString("empty"))
    }

    def toJson(node: LHS): JsValue = node match {
      case This(_) => JsObject("name" -> JsString("this"))
      case Null(_) => JsObject("name" -> JsString("null"))
      case Undefined(_) => JsObject("name" -> JsString("undefined"))
      case Bool(_, isBool) => JsObject(
        "name" -> JsString(if (isBool) "true" else "false")
      )
      case n: NumberLiteral => toJson(n)
      case StringLiteral(_, quote, escaped, _) => JsObject(
        "name" -> JsString(quote + escaped + quote)
      )
      case RegularExpression(_, body, flag) => JsObject(
        "name" -> JsString("/" + body + "/" + flag)
      )
      case VarRef(_, id) => JsObject(
        "name" -> JsString("var"),
        "children" -> JsArray(toJson(id))
      )
      case ArrayExpr(_, elements) => JsObject(
        "name" -> JsString("array"),
        "children" -> JsArray(elements.map(_.map(toJson).getOrElse(JsNull)).to[Vector])
      )
      case ArrayNumberExpr(_, elements) => JsObject(
        "name" -> JsString("number array"),
        "children" -> JsArray(elements.map(d => JsObject(
          "name" -> JsString(d.toString)
        )).to[Vector])
      )
      case ObjectExpr(_, members) => JsObject(
        "name" -> JsString("object"),
        "children" -> JsArray(members.map(toJson).to[Vector])
      )
      case Parenthesized(_, expr) => JsObject(
        "name" -> JsString("parenthesized"),
        "children" -> JsArray(toJson(expr))
      )
      case FunExpr(_, ftn) => JsObject(
        "name" -> JsString("function"),
        "children" -> JsArray(toJson(ftn))
      )
      case Bracket(_, obj, index) => JsObject(
        "name" -> JsString("bracket"),
        "children" -> JsArray(toJson(obj), toJson(index))
      )
      case Dot(_, obj, member) => JsObject(
        "name" -> JsString("dot"),
        "children" -> JsArray(toJson(obj), toJson(member))
      )
      case New(_, lhs) => JsObject(
        "name" -> JsString("new"),
        "children" -> JsArray(toJson(lhs))
      )
      case FunApp(_, fun, args) => JsObject(
        "name" -> JsString("function call"),
        "children" -> JsArray(toJson(fun) +: args.map(toJson).to[Vector])
      )
    }

    def toJson(node: NumberLiteral): JsValue = node match {
      case DoubleLiteral(_, text, _) => JsObject("name" -> JsString(text))
      case IntLiteral(_, intVal, _) => JsObject("name" -> JsString(intVal.toString))
    }

    def toJson(node: SourceElement): JsValue = node match {
      case s: Stmt => toJson(s)
    }

    def toJson(node: SourceElements): JsValue = node match {
      case SourceElements(_, body, isStrict) => JsObject(
        "name" -> JsString(if (isStrict) "strict " else "" + "source elements"),
        "children" -> JsArray(body.map(toJson).to[Vector])
      )
    }

    def toJson(node: FunDecl): JsValue = node match {
      case FunDecl(_, ftn, isStrict) => JsObject(
        "name" -> JsString(if (isStrict) "strict " else "" + "function"),
        "children" -> JsArray(toJson(ftn))
      )
    }

    def toJson(node: VarDecl): JsValue = node match {
      case VarDecl(_, name, expr, isStrict) => JsObject(
        "name" -> JsString(if (isStrict) "strict " else "" + "variable"),
        "children" -> JsArray(toJson(name), expr.map(toJson).getOrElse(JsNull))
      )
    }

    def toJson(node: Case): JsValue = node match {
      case Case(_, cond, body) => JsObject(
        "name" -> JsString("case"),
        "children" -> JsArray(toJson(cond) +: body.map(toJson).to[Vector])
      )
    }

    def toJson(node: Catch): JsValue = node match {
      case Catch(_, id, body) => JsObject(
        "name" -> JsString("catch"),
        "children" -> JsArray(toJson(id) +: body.map(toJson).to[Vector])
      )
    }

    def toJson(node: Property): JsValue = node match {
      case PropId(_, id) => JsObject(
        "name" -> JsString("identifier"),
        "children" -> JsArray(toJson(id))
      )
      case PropStr(_, str) => JsObject(
        "name" -> JsString(str)
      )
      case PropNum(_, num) => JsObject(
        "name" -> JsString("number"),
        "children" -> JsArray(toJson(num))
      )
    }

    def toJson(node: Member): JsValue = node match {
      case Field(_, prop, expr) => JsObject(
        "name" -> JsString("field"),
        "children" -> JsArray(toJson(prop), toJson(expr))
      )
      case GetProp(_, prop, ftn) => JsObject(
        "name" -> JsString("getter"),
        "children" -> JsArray(toJson(prop), toJson(ftn))
      )
      case SetProp(_, prop, ftn) => JsObject(
        "name" -> JsString("setter"),
        "children" -> JsArray(toJson(prop), toJson(ftn))
      )
    }

    def toJson(node: Id): JsValue = node match {
      case Id(_, text, _, _) => JsObject("name" -> JsString(text))
    }

    def toJson(node: Op): JsValue = node match {
      case Op(_, text) => JsObject("name" -> JsString(text))
    }

    def toJson(node: AnonymousFnName): JsValue = node match {
      case AnonymousFnName(_, text) => JsObject("name" -> JsString(text))
    }

    def toJson(node: Label): JsValue = node match {
      case Label(_, id) => JsObject(
        "name" -> JsString("label"),
        "children" -> JsArray(toJson(id))
      )
    }

    def toJson(node: Comment): JsValue = node match {
      case Comment(_, comment) => JsObject(
        "name" -> JsString("comment"),
        "children" -> JsArray(JsString(comment))
      )
    }

    def toJson(node: TopLevel): JsValue = node match {
      case TopLevel(_, fds, vds, stmts) => JsObject(
        "name" -> JsString("top level"),
        "children" -> JsArray(fds.map(toJson).to[Vector] ++
          vds.map(toJson) ++ stmts.map(toJson))
      )
    }

    def toJson(node: Functional): JsValue = node match {
      case Functional(_, fds, vds, stmts, name, params, _) => JsObject(
        "name" -> JsString("functional"),
        "children" -> JsArray((fds.map(toJson).to[Vector] ++ vds.map(toJson) :+
          toJson(stmts) :+ toJson(name)) ++ params.map(toJson))
      )
    }

    def write(node: ASTNode): JsValue = node match {
      case p: Program => toJson(p)
      case s: SourceElement => toJson(s)
      case s: SourceElements => toJson(s)
      case v: VarDecl => toJson(v)
      case c: Case => toJson(c)
      case c: Catch => toJson(c)
      case e: Expr => toJson(e)
      case p: Property => toJson(p)
      case m: Member => toJson(m)
      case i: Id => toJson(i)
      case o: Op => toJson(o)
      case a: AnonymousFnName => toJson(a)
      case l: Label => toJson(l)
      case c: Comment => toJson(c)
      case t: TopLevel => toJson(t)
      case f: Functional => toJson(f)
    }

    def read(value: JsValue): ASTNode = throw new DeserializationException("JSON read not supported")
  }
}
