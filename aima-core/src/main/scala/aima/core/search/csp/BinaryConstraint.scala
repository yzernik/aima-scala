/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

package aima.core.search.csp

case class BinaryConstraint[A, B](scope: (Variable[A], Variable[B]), rel: (A, B) => Boolean) {
  lazy val reverse = BinaryConstraint[B, A](scope.swap, (b, a) => rel(a, b))

  def contains(variable: Variable[_]): Boolean = scope._1 == variable || scope._2 == variable

  def countRuledOut(leftAssignment: A, csp: CSP): Int =
    csp getCurrentDomain scope._2 count {rightAssignment => !rel(leftAssignment, rightAssignment)}

  def onRight[C](variable: Variable[C]): Option[BinaryConstraint[_, C]] = this match {
    case onRight: BinaryConstraint[_, C] if scope._2 == variable => Some(onRight)
    case onLeft: BinaryConstraint[C, _] if scope._1 == variable => reverse.onRight[C](variable)
    case _ => None
  }

  def onLeft[C](variable: Variable[C]): Option[BinaryConstraint[C, _]] = this match {
    case onRight: BinaryConstraint[_, C] if scope._2 == variable => reverse.onLeft[C](variable)
    case onLeft: BinaryConstraint[C, _] if scope._1 == variable => Some(onLeft)
    case _ => None
  }
}
