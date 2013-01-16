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

package aima.core.probability.bayes

import aima.core.probability.impl.{FiniteRandomVariable, AssignmentProposition}
import aima.core.probability._
import scala.annotation.tailrec
import aima.core.probability.impl.FiniteRandomVariable
import aima.core.probability.impl.AssignmentProposition

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 512.<br>
 * <br>
 * A Conditional Probability Table, or CPT, can be used for representing
 * conditional probabilities for discrete (finite) random variables. Each row in
 * a CPT contains the conditional probability of each node value for a
 * <b>conditioning case</b>.
 *
 * @author Alex DiCarlo
 */
trait ConditionalProbabilityTable[A] extends ConditionalProbabilityDistribution[A] {
  override def variable: FiniteRandomVariable[A]

  override def parents: List[FiniteRandomVariable[_]]

  override lazy val variables: List[FiniteRandomVariable[_]] = parents :+ variable

  override def conditioningCaseFor(parentValues: List[AssignmentProposition]): CategoricalDistribution

  override def sample(probabilityChoice: Double, parentValues: List[AssignmentProposition]): A =
    bayes.sample(variable, probabilityChoice, parentValues, conditioningCaseFor(parentValues).values)

  /**
   * Construct a Factor consisting of the Random Variables from the
   * Conditional Probability Table that are not part of the evidence (see
   * AIMA3e pg. 524).
   *
   * @param evidence
   * @return a Factor for the Random Variables from the Conditional
   *         Probability Table that are not part of the evidence.
   */
  def factorFor(evidence: List[AssignmentProposition]): Factor
}