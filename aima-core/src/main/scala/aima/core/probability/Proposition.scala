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

package aima.core.probability

import aima.core.probability.World.AnyWorld

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 486.<br>
 * <br>
 * Propositions describing sets of possible worlds are written in a notation
 * that combines elements of propositional logic and constraint satisfaction
 * notation. In the terminology of Section 2.4.7, it is a factored
 * representation, in which a possible world is represented by a set of
 * variable/value pairs.<br>
 * <br>
 * A possible world is defined to be an assignment of values to all of the
 * random variables under consideration.
 *
 * @author Alex DiCarlo
 */
trait Proposition {
  /**
   * The set of RandomVariables in the World (sample space) that this Proposition is applicable to.
   */
  def scope: Set[RandomVariable[_]]
  /**
   * Determine whether or not the proposition holds in a particular possible world.
   *
   * @param possibleWorld A possible world is defined to be an assignment of values to all of the random
   *                      variables under consideration.
   * @return true if the proposition holds in the given possible world, false otherwise.
   */
  def holdsIn(possibleWorld: AnyWorld): Boolean
}

/**
 * A proposition on a single variable term.
 *
 * Note: The scope may be greater than a single variable as the term may be a
 * derived variable (e.g. Total=Dice1+Dice2).
 */
trait TermProposition extends Proposition {
  /**
   * The Term's Variable.
   */
  def variable: RandomVariable[_]
  lazy val scope: Set[RandomVariable[_]] = Set(variable)
  def holdsIn(possibleWorld: AnyWorld): Boolean = possibleWorld.get(variable).isDefined
}

object TermProposition {
  def unapply(prop: TermProposition): Option[RandomVariable[_]] = Some(prop.variable)
}

abstract class AtomicProposition(val scope: Set[RandomVariable[_]]) extends Proposition
abstract class ComplexProposition(val propositions: Set[Proposition]) extends Proposition {
  val scope: Set[RandomVariable[_]] = propositions flatMap {_.scope}
}