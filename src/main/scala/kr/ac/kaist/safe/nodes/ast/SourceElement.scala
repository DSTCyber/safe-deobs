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

trait SourceElement extends ASTNode

// Program ::= SourceElement*
case class SourceElements(
    info: ASTNodeInfo,
    body: List[SourceElement],
    strict: Boolean
) extends ASTNode {
  override def toString(indent: Int): String = ""

  override def =~(that: ASTNode): Boolean = (this, that) match {
    case (SourceElements(_, body1, strict1), SourceElements(_, body2, strict2)) =>
      NU.fuzzyCompare(body1, body2) && strict1 == strict2
    case _ => false
  }
}
