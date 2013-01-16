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

import aima.core.probability._
import aima.core.probability.World.AnyWorld
import scala.Some

case class AssignmentProposition(assign: SingleAssignment[_]) extends AtomicProposition(Set(assign.variable)) {
  def holdsIn(possibleWorld: AnyWorld): Boolean = possibleWorld get assign.variable match {
    case Some(SingleAssignment(_, value)) => assign.value == value
    case _ => false
  }
}
case class SubsetProposition(assign: SetAssignment[_]) extends AtomicProposition(Set(assign.variable)) {
  def holdsIn(possibleWorld: AnyWorld): Boolean = possibleWorld get assign.variable match {
    case Some(possibleAssign) => possibleAssign ⊆ assign
    case _ => false
  }
}
case class EquivalentProposition(variables: Set[RandomVariable[_]]) extends AtomicProposition(variables) {
  require(variables.size >= 2)
  def holdsIn(possibleWorld: AnyWorld): Boolean = (variables groupBy {possibleWorld get _}).keySet.size == 1
}

case class ¬(proposition: Proposition) extends ComplexProposition(Set(proposition)) {
  def holdsIn(possibleWorld: AnyWorld): Boolean = !(proposition holdsIn possibleWorld)
}
sealed abstract class BinarySentenceProposition(left: Proposition, right: Proposition)
  extends ComplexProposition(Set(left, right)) {
  def holdsIn(possibleWorld: AnyWorld): Boolean = operator(left holdsIn possibleWorld, right holdsIn possibleWorld)
  def operator(left: Boolean, right: Boolean): Boolean
}
object Connective {
  case class ∧(left: Proposition, right: Proposition) extends BinarySentenceProposition(left, right) {
    def operator(left: Boolean, right: Boolean): Boolean = left && right
  }
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): Formula 13.4, page 489.<br>
   * <br>
   *
   * We can also derive the well-known formula for the probability of a disjunction, sometimes called the
   * <b>inclusion-exclusion principle:</b><br> <br> P(a OR b) = P(a) + P(b) - P(a AND b).<br>
   */
  case class ∨(left: Proposition, right: Proposition) extends BinarySentenceProposition(left, right) {
    def operator(left: Boolean, right: Boolean): Boolean = left || right
  }
}

object Proposition {
  implicit final class ProbabilityProposition(left: Proposition) {
    @inline def ∧(right: Proposition): Connective.∧ = Connective.∧(left, right)
    @inline def ∨(right: Proposition): Connective.∨ = Connective.∨(left, right)
  }

  implicit def variableToTermProposition(_variable: RandomVariable[_]): TermProposition = new TermProposition {
    val variable: RandomVariable[_] = _variable
  }
}

