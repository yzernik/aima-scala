/*
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

package aima.core.probability.impl

import aima.core.probability.{Assignment, RandomVariable}

object Assignment {
  def unapply[A](assignment: Assignment[A]): Option[(RandomVariable[A], Set[A])] = {
    Some(assignment.variable, assignment.values)
  }
}

case class SingleAssignment[A](variable: RandomVariable[A], value: A) extends Assignment[A] {
  val values: Set[A] = Set(value)
}
case class SetAssignment[A](variable: RandomVariable[A], values: Set[A]) extends Assignment[A]
