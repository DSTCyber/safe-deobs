/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.nodes.ast

import java.lang.Double
import java.math.BigInteger
import kr.ac.kaist.safe.util.{ NodeUtil => NU }
import kr.ac.kaist.safe.LINE_SEP

trait Expr extends ASTNode {
  def isEval: Boolean = this match {
    case VarRef(info, Id(_, text, _, _)) => text.equals("eval")
    case _ => false
  }

  def unwrapParen: Expr = this match {
    case Parenthesized(info, body) => body
    case _ => this
  }
}

// Expr ::= Expr, Expr
case class ExprList(
    info: ASTNodeInfo,
    exprs: List[Expr]
) extends Expr {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    NU.join(indent, exprs, ", ", s)
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ExprList(_, exprs1), ExprList(_, exprs2)) =>
      NU.fuzzyCompare(exprs1, exprs2)
    case _ => false
  }
}

// Expr ::= Expr ? Expr : Expr
case class Cond(
    info: ASTNodeInfo,
    cond: Expr,
    trueBranch: Expr,
    falseBranch: Expr
) extends Expr {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(cond.toString(indent))
      .append(" ? ")
      .append(trueBranch.toString(indent))
      .append(" : ")
      .append(falseBranch.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Cond(_, cond1, trueBranch1, falseBranch1), Cond(_, cond2, trueBranch2, falseBranch2)) =>
      cond1 =~ cond2 && trueBranch1 =~ trueBranch2 && falseBranch1 =~ falseBranch2
    case _ => false
  }
}

// Expr ::= Expr Op Expr
case class InfixOpApp(
    info: ASTNodeInfo,
    left: Expr,
    op: Op,
    right: Expr
) extends Expr {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(left.toString(indent))
      .append(" ")
      .append(op.toString(indent))
      .append(" ")
      .append(right.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (InfixOpApp(_, left1, op1, right1), InfixOpApp(_, left2, op2, right2)) =>
      left1 =~ left2 && op1 =~ op2 && right1 =~ right2
    case _ => false
  }
}

// Expr ::= Op Expr
case class PrefixOpApp(
    info: ASTNodeInfo,
    op: Op,
    right: Expr
) extends Expr {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(op.toString(indent))
      .append(" ")
      .append(right.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (PrefixOpApp(_, op1, right1), PrefixOpApp(_, op2, right2)) =>
      op1 =~ op2 && right1 =~ right2
    case _ => false
  }
}

// Expr ::= Lhs Op
case class UnaryAssignOpApp(
    info: ASTNodeInfo,
    lhs: LHS,
    op: Op
) extends Expr {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(lhs.toString(indent))
      .append(op.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (UnaryAssignOpApp(_, lhs1, op1), UnaryAssignOpApp(_, rhs2, op2)) =>
      lhs1 =~ rhs2 && op1 =~ op2
    case _ => false
  }
}

// Expr ::= Lhs Op Expr
case class AssignOpApp(
    info: ASTNodeInfo,
    lhs: LHS,
    op: Op,
    right: Expr
) extends Expr {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(lhs.toString(indent))
      .append(" ")
      .append(op.toString(indent))
      .append(" ")
      .append(right.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (AssignOpApp(_, lhs1, op1, right1), AssignOpApp(_, lhs2, op2, right2)) =>
      lhs1 =~ lhs2 && op1 =~ op2 && right1 =~ right2
    case _ => false
  }
}

// Expr ::= Lhs
trait LHS extends Expr {
  def isName: Boolean = this match {
    case _: VarRef => true
    case _: Dot => true
    case _ => false
  }
}

// Lhs ::= Literal
trait Literal extends LHS

// Literal ::= this
case class This(
    info: ASTNodeInfo
) extends Literal {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("this")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (This(_), This(_)) => true
    case _ => false
  }
}

// Literal ::= null
case class Null(
    info: ASTNodeInfo
) extends Literal {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("null")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Null(_), Null(_)) => true
    case _ => false
  }
}

// Literal ::= undefined
case class Undefined(
    override val info: ASTNodeInfo
) extends Literal(info: ASTNodeInfo) {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("undefined")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Undefined(_), Undefined(_)) => true
    case _ => false
  }
}

// Literal ::= true | false
case class Bool(
    info: ASTNodeInfo, bool: Boolean
) extends Literal {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(if (bool) "true" else "false")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Bool(_, bool1), Bool(_, bool2)) => bool1 == bool2
    case _ => false
  }
}

// number literal
trait NumberLiteral extends Literal

// float literal
case class DoubleLiteral(
    info: ASTNodeInfo,
    text: String,
    num: Double
) extends NumberLiteral {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(text)
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (DoubleLiteral(_, text1, num1), DoubleLiteral(_, text2, num2)) =>
      text1 == text2 && num1 == num2
    case _ => false
  }
}

// int literal
case class IntLiteral(
    info: ASTNodeInfo,
    intVal: BigInteger,
    radix: Integer
) extends NumberLiteral {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(radix.toInt match {
      case 8 => "0" + intVal.toString(8)
      case 16 => "0x" + intVal.toString(16)
      case _ => intVal.toString
    })
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (IntLiteral(_, intVal1, _), IntLiteral(_, intVal2, _)) =>
      intVal1.equals(intVal2)
    case _ => false
  }
}

// Literal ::= String
case class StringLiteral(
    info: ASTNodeInfo,
    quote: String,
    escaped: String,
    isRE: Boolean
) extends Literal {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(quote)
    NU.ppAST(s, escaped)
    s.append(quote)
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (StringLiteral(_, _, escaped1, isRE1), StringLiteral(_, _, escaped2, isRE2)) =>
      escaped1 == escaped2 && isRE1 == isRE2
    case _ => false
  }
}

// Literal ::= RegularExpression
case class RegularExpression(
    info: ASTNodeInfo,
    body: String,
    flag: String
) extends Literal {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("/" + body + "/" + flag)
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (RegularExpression(_, body1, flag1), RegularExpression(_, body2, flag2)) =>
      body1 == body2 && flag1 == flag2
    case _ => false
  }
}

// PrimaryExpr ::= Id
case class VarRef(
    info: ASTNodeInfo,
    id: Id
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(id.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (VarRef(_, id1), VarRef(_, id2)) => id1 =~ id2
    case _ => false
  }
}

// PrimaryExpr ::= [ (Expr,)* ]
case class ArrayExpr(
    info: ASTNodeInfo,
    elements: List[Option[Expr]]
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("[")
    elements.foreach(e => s.append(e.fold("") {
      _.toString(indent)
    }).append(", "))
    s.append("]")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ArrayExpr(_, elements1), ArrayExpr(_, elements2)) =>
      NU.fuzzyCompare(elements1.flatten, elements2.flatten)
    case _ => false
  }
}

// PrimaryExpr ::= [ (Number,)* ]
case class ArrayNumberExpr(
    info: ASTNodeInfo,
    elements: List[Double]
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("[")
    s.append("\"A LOT!!! " +
      elements.size +
      " elements are not printed here.\", ")
    s.append("]")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ArrayNumberExpr(_, elements1), ArrayNumberExpr(_, elements2)) =>
      elements1 == elements2
    case _ => false
  }
}

// PrimaryExpr ::= { (Member,)* }
case class ObjectExpr(
    info: ASTNodeInfo,
    members: List[Member]
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("{")
      .append(LINE_SEP)
      .append(NU.getIndent(indent + 1))
      .append(NU.join(
        indent + 1,
        members,
        "," + LINE_SEP + NU.getIndent(indent + 1),
        new StringBuilder("")
      ))
      .append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (ObjectExpr(_, members1), ObjectExpr(_, members2)) =>
      NU.fuzzyCompare(members1, members2)
    case _ => false
  }
}

// PrimaryExpr ::= ( Expr )
case class Parenthesized(
    info: ASTNodeInfo,
    expr: Expr
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(NU.inParentheses(expr.toString(indent)))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Parenthesized(_, expr1), Parenthesized(_, expr2)) => expr1 =~ expr2
    case _ => false
  }
}

// LHS ::= function Id? ( (Id,)* ) { SourceElement }
case class FunExpr(
    info: ASTNodeInfo,
    ftn: Functional
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("function ")
    if (!NU.isFunExprName(ftn.name.text)) s.append(ftn.name.toString(indent))
    s.append("(")
      .append(NU.join(
        indent,
        ftn.params,
        ", ",
        new StringBuilder("")
      ))
      .append(") {")
      .append(LINE_SEP)
    NU.prUseStrictDirective(s, indent, ftn.fds, ftn.vds, ftn.stmts)
    NU.prFtn(s, indent, ftn.fds, ftn.vds, ftn.stmts.body)
    s.append(LINE_SEP)
      .append(NU.getIndent(indent))
      .append("}")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (FunExpr(_, ftn1), FunExpr(_, ftn2)) => ftn1 =~ ftn2
    case _ => false
  }
}

// LHS ::= Lhs [ Expr ]
case class Bracket(
    info: ASTNodeInfo,
    obj: LHS,
    index: Expr
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(obj.toString(indent))
      .append("[")
      .append(index.toString(indent))
      .append("]")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Bracket(_, obj1, index1), Bracket(_, obj2, index2)) =>
      obj1 =~ obj2 && index1 =~ index2
    case _ => false
  }
}

// LHS ::= Lhs . Id
case class Dot(
    info: ASTNodeInfo,
    obj: LHS,
    member: Id
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(obj.toString(indent))
      .append(".")
      .append(member.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Dot(_, obj1, member1), Dot(_, obj2, member2)) =>
      obj1 =~ obj2 && member1 =~ member2
    case _ => false
  }
}

// LHS ::= new Lhs
case class New(
    info: ASTNodeInfo,
    lhs: LHS
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append("new ")
      .append(lhs.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (New(_, lhs1), New(_, lhs2)) => lhs1 =~ lhs2
    case _ => false
  }
}

// LHS ::= Lhs ( (Expr,)* )
case class FunApp(
    info: ASTNodeInfo,
    fun: LHS,
    args: List[Expr]
) extends LHS {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(fun.toString(indent))
      .append("(")
      .append(NU.join(indent, args, ", ", new StringBuilder("")))
      .append(")")
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (FunApp(_, fun1, args1), FunApp(_, fun2, args2)) =>
      fun1 =~ fun2 && NU.fuzzyCompare(args1, args2)
    case _ => false
  }
}

// LHS ::= function Id (.Id)+ ( (Id,)* ) { SourceElement }
case class JScriptMemFunExpr(
    override val info: ASTNodeInfo,
    obj: Id,
    members: List[Id],
    ftn: Functional
) extends LHS(info: ASTNodeInfo) {
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
    case (JScriptMemFunExpr(_, obj1, members1, ftn1), JScriptMemFunExpr(_, obj2, members2, ftn2)) =>
      obj1 =~ obj2 && NU.fuzzyCompare(members1, members2) && ftn1 =~ ftn2
    case _ => false
  }
}
