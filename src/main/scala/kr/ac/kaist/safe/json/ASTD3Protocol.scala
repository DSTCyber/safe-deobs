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
    private def toJsonWithoutNulls(seq: Seq[ASTNode]): Vector[JsValue] =
      seq.map(toJson).filterNot(_ == JsNull).to[Vector]

    private def toJsonWithoutNulls(opt: Option[ASTNode]): Vector[JsValue] =
      opt.map(toJson) match {
        case Some(json) => Vector(json)
        case None => Vector()
      }

    private def toJson(node: Program): JsValue = node match {
      case Program(_, TopLevel(_, fds, vds, stmts)) => JsObject(
        "name" -> JsString("program"),
        "children" -> JsArray(toJsonWithoutNulls(fds) ++
          toJsonWithoutNulls(vds) ++ toJsonWithoutNulls(stmts))
      )
    }

    private def toJson(node: Stmt): JsValue = node match {
      case _: NoOp => JsNull
      case StmtUnit(_, stmts) => JsObject(
        "name" -> JsString("stmts"),
        "children" -> JsArray(toJsonWithoutNulls(stmts))
      )
      case fd: FunDecl => toJson(fd)
      case ABlock(_, stmts, _) => JsObject(
        "name" -> JsString("stmts"),
        "children" -> JsArray(toJsonWithoutNulls(stmts))
      )
      case VarStmt(_, List(vd)) => toJson(vd)
      case VarStmt(_, vds) => JsObject(
        "name" -> JsString("vars"),
        "children" -> JsArray(toJsonWithoutNulls(vds))
      )
      case EmptyStmt(_) => JsObject("name" -> JsString("empty"))
      case ExprStmt(_, expr, _) => JsObject(
        "name" -> JsString("expr"),
        "children" -> JsArray(toJson(expr))
      )
      case If(_, cond, trueB, falseB) => JsObject(
        "name" -> JsString("if"),
        "children" -> JsArray(toJson(cond) +: toJson(trueB) +:
          toJsonWithoutNulls(falseB))
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
        "name" -> JsString("for"),
        "children" -> JsArray(
          toJsonWithoutNulls(vars) :+ cond.map(toJson).getOrElse(JsNull) :+
            action.map(toJson).getOrElse(JsNull) :+ toJson(body)
        )
      )
      case ForVarIn(_, vari, expr, body) => JsObject(
        "name" -> JsString("for"),
        "children" -> JsArray(toJson(vari), toJson(expr), toJson(body))
      )
      case Continue(_, target) => JsObject(
        "name" -> JsString("continue"),
        "children" -> JsArray(toJsonWithoutNulls(target))
      )
      case Break(_, target) => JsObject(
        "name" -> JsString("break"),
        "children" -> JsArray(toJsonWithoutNulls(target))
      )
      case Return(_, expr) => JsObject(
        "name" -> JsString("return"),
        "children" -> JsArray(toJsonWithoutNulls(expr))
      )
      case With(_, expr, stmt) => JsObject(
        "name" -> JsString("with"),
        "children" -> JsArray(toJson(expr), toJson(stmt))
      )
      case Switch(_, cond, frontCases, defi, backCases) => JsObject(
        "name" -> JsString("switch"),
        "children" -> JsArray((toJson(cond) +:
          toJsonWithoutNulls(frontCases)) ++
          defi.map(_.map(toJson)).getOrElse(Vector()) ++
          toJsonWithoutNulls(backCases))
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
        "children" -> JsArray((toJsonWithoutNulls(body) :+
          catchBlock.map(toJson).getOrElse(JsNull)) ++
          fin.map(_.map(toJson)).getOrElse(Vector()))
      )
      case Debugger(_) => JsObject("name" -> JsString("debugger"))
    }

    private def toJson(node: Expr): JsValue = node match {
      case ExprList(_, exprs) => JsObject(
        "name" -> JsString("exprs"),
        "children" -> JsArray(toJsonWithoutNulls(exprs))
      )
      case Cond(_, cond, trueB, falseB) => JsObject(
        "name" -> JsString("cond"),
        "children" -> JsArray(toJson(cond), toJson(trueB), toJson(falseB))
      )
      case InfixOpApp(_, left, op, right) => JsObject(
        "name" -> JsString("op"),
        "children" -> JsArray(toJson(left), toJson(op), toJson(right))
      )
      case PrefixOpApp(_, op, right) => JsObject(
        "name" -> JsString("op"),
        "children" -> JsArray(toJson(op), toJson(right))
      )
      case UnaryAssignOpApp(_, lhs, op) => JsObject(
        "name" -> JsString("assign"),
        "children" -> JsArray(toJson(lhs), toJson(op))
      )
      case AssignOpApp(_, lhs, op, right) => JsObject(
        "name" -> JsString("assign"),
        "children" -> JsArray(toJson(lhs), toJson(op), toJson(right))
      )
      case l: LHS => toJson(l)
      case EmptyExpr(_) => JsObject("name" -> JsString("empty"))
    }

    private def toJson(node: LHS): JsValue = node match {
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
        "children" -> JsArray(toJsonWithoutNulls(members))
      )
      case Parenthesized(_, expr) => JsObject(
        "name" -> JsString("parenthesized"),
        "children" -> JsArray(toJson(expr))
      )
      case FunExpr(_, ftn) => JsObject(
        "name" -> JsString("func"),
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
        "name" -> JsString("func call"),
        "children" -> JsArray(toJson(fun) +: toJsonWithoutNulls(args))
      )
    }

    private def toJson(node: NumberLiteral): JsValue = node match {
      case DoubleLiteral(_, text, _) => JsObject("name" -> JsString(text))
      case IntLiteral(_, intVal, _) => JsObject("name" -> JsString(intVal.toString))
    }

    private def toJson(node: SourceElement): JsValue = node match {
      case s: Stmt => toJson(s)
    }

    private def toJson(node: SourceElements): JsValue = node match {
      case SourceElements(_, body, isStrict) => JsObject(
        "name" -> JsString(if (isStrict) "strict " else "" + "src elems"),
        "children" -> JsArray(toJsonWithoutNulls(body))
      )
    }

    private def toJson(node: FunDecl): JsValue = node match {
      case FunDecl(_, Functional(_, fds, vds, stmts, name, params, _), isStrict) => JsObject(
        "name" -> JsString(if (isStrict) "strict " else "" + "func"),
        "children" -> JsArray((toJsonWithoutNulls(fds) ++
          toJsonWithoutNulls(vds) :+ toJson(stmts) :+ toJson(name)) ++
          toJsonWithoutNulls(params))
      )
    }

    private def toJson(node: VarDecl): JsValue = node match {
      case VarDecl(_, name, expr, isStrict) => JsObject(
        "name" -> JsString(if (isStrict) "strict " else "" + "var"),
        "children" -> JsArray(toJson(name) +: toJsonWithoutNulls(expr))
      )
    }

    private def toJson(node: Case): JsValue = node match {
      case Case(_, cond, body) => JsObject(
        "name" -> JsString("case"),
        "children" -> JsArray(toJson(cond) +: toJsonWithoutNulls(body))
      )
    }

    private def toJson(node: Catch): JsValue = node match {
      case Catch(_, id, body) => JsObject(
        "name" -> JsString("catch"),
        "children" -> JsArray(toJson(id) +: toJsonWithoutNulls(body))
      )
    }

    private def toJson(node: Property): JsValue = node match {
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

    private def toJson(node: Member): JsValue = node match {
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

    private def toJson(node: Id): JsValue = node match {
      case Id(_, text, _, _) => JsObject("name" -> JsString(text))
    }

    private def toJson(node: Op): JsValue = node match {
      case Op(_, text) => JsObject("name" -> JsString(text))
    }

    private def toJson(node: AnonymousFnName): JsValue = node match {
      case AnonymousFnName(_, text) => JsObject("name" -> JsString(text))
    }

    private def toJson(node: Label): JsValue = node match {
      case Label(_, id) => JsObject(
        "name" -> JsString("label"),
        "children" -> JsArray(toJson(id))
      )
    }

    private def toJson(node: Comment): JsValue = node match {
      case Comment(_, comment) => JsObject(
        "name" -> JsString("comment"),
        "children" -> JsArray(JsString(comment))
      )
    }

    private def toJson(node: TopLevel): JsValue = node match {
      case TopLevel(_, fds, vds, stmts) => JsObject(
        "name" -> JsString("top level"),
        "children" -> JsArray(toJsonWithoutNulls(fds) ++
          toJsonWithoutNulls(vds) ++ toJsonWithoutNulls(stmts))
      )
    }

    private def toJson(node: Functional): JsValue = node match {
      case Functional(_, fds, vds, stmts, name, params, _) => JsObject(
        "name" -> JsString("functional"),
        "children" -> JsArray((toJsonWithoutNulls(fds) ++
          toJsonWithoutNulls(vds) :+ toJson(stmts) :+ toJson(name)) ++
          toJsonWithoutNulls(params))
      )
    }

    private def toJson(node: ASTNode): JsValue = node match {
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

    def write(node: ASTNode): JsValue = toJson(node)

    def read(value: JsValue): ASTNode =
      throw new DeserializationException("JSON read not supported")
  }
}
