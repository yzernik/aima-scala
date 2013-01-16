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

import aima.core.probability.impl.AssignmentProposition

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 487.<br>
 * <br>
 * A probability distribution is a function that assigns probabilities to events
 * (sets of possible worlds).<br>
 * <br>
 * <b>Note:</b> This definition is slightly different than that given in AIMA3e
 * pg. 487, which in this API corresponds to a CategoricalDistribution.
 *
 * @see <a href="http://en.wikipedia.org/wiki/Probability_distribution" >Probability Distribution</a>
 *
 * @author Alex DiCarlo
 */
trait ProbabilityDistribution {
  /**
   * @return variables that make up this distribution
   */
  def variables: List[RandomVariable[_]]
  /**
   * Get the value for the provided set of AssignmentPropositions for the random variables
   * comprising the Distribution (size of each must equal and their random variables must match).
   *
   * @param assignments the assignment propositions for the random variables
   *                               comprising the Distribution
   * @return the value for the possible worlds associated with the assignments for the
   *         random variables comprising the Distribution.
   */
  def value(assignments: List[AssignmentProposition]): Double
}
