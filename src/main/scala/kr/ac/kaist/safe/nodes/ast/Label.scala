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

// label
case class Label(
    info: ASTNodeInfo,
    id: Id
) extends ASTNode {
  override def toString(indent: Int): String = {
    val s: StringBuilder = new StringBuilder
    comment.map(c => s.append(c.toString(indent)))
    s.append(id.toString(indent))
    s.toString
  }

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (Label(_, id1), Label(_, id2)) => id1 =~ id2
    case _ => false
  }
}
