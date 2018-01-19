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

import kr.ac.kaist.safe.util.{ NodeUtil => NU }
import kr.ac.kaist.safe.SIGNIFICANT_BITS

trait Name extends ASTNode

trait IdOrOpOrAnonymousName extends Name

trait IdOrOp extends IdOrOpOrAnonymousName {
  val text: String
}

// Named identifier
case class Id(
    info: ASTNodeInfo,
    text: String,
    uniqueName: Option[String] = None,
    isWith: Boolean
) extends IdOrOp {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    uniqueName match {
      case Some(u) if isWith =>
        s.append(u.dropRight(SIGNIFICANT_BITS) +
          NU.getNodesE(u.takeRight(SIGNIFICANT_BITS)))
      case _ => s.append(text)
    }
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Id(_, text1, uniqueName1, isWith1), Id(_, text2, uniqueName2, isWith2)) =>
      text1 == text2 && uniqueName1 == uniqueName2 && isWith1 == isWith2
    case _ => false
  }
}

// Infix/prefix/postfix operator
case class Op(
    info: ASTNodeInfo,
    text: String
) extends IdOrOp {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(text)
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Op(_, text1), Op(_, text2)) => text1 == text2
    case _ => false
  }
}

// Unnamed identifier
case class AnonymousFnName(
    info: ASTNodeInfo,
    text: String
) extends IdOrOpOrAnonymousName {
  override def toString(indent: Int): String = ""

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (AnonymousFnName(_, text1), AnonymousFnName(_, text2)) =>
      text1 == text2
    case _ => false
  }
}
