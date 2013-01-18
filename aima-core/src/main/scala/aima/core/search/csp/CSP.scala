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

case class CSP(assignments: List[Assignment[_]], constraints: List[BinaryConstraint[_, _]]) {
  def neighbors(variable: Variable[_]): List[BinaryConstraint[_, _]] = constraints filter {_ contains variable}

  def constraintsSatisfied[A](assign: Assignment[A]): Boolean = {
    neighbors(assign.variable) forall {
      constraint =>
        assign.values forall {
          value =>
            (constraint onLeft assign.variable map {_.countRuledOut(value, this)} getOrElse 0) == 0
        }
    }
  }

  def allConstraintsSatisfied(): Boolean = assignments forall {constraintsSatisfied(_)}

  def conflictingAssignments(): List[Assignment[_]] = assignments filterNot {constraintsSatisfied(_)}

  def getAssignment[A](variable: Variable[A]): Option[Assignment[A]] = {
    assignments.flatMap[Assignment[A], List[Assignment[A]]] {
      case assign: Assignment[A] if variable == assign.variable => Some(assign)
      case _ => None
    }.headOption
  }

  def getCurrentDomain[A](variable: Variable[A]): Set[A] = getAssignment(variable) map {_.values} getOrElse Set()

  def reduceDomain[A](assign: Assignment[A], value: A): CSP =
    this.copy(assignments = assign.copy(values = Set(value)) :: (assignments filterNot {_ == assign}))
}
