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

import aima.core.probability.impl.AssignmentProposition
import aima.core.probability.{CategoricalDistribution, RandomVariable}

/**
 * General interface to be implemented by Bayesian Inference algorithms.
 *
 * @author Alex DiCarlo
 */
trait BayesianInference {
  /**
   * @param X the query variables.
   * @param evidence observed values for variables E.
   * @param network a Bayes net with variables {X} &cup; E &cup; Y / Y = hidden variables
   * @return a distribution over the query variables.
   */
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): CategoricalDistribution
}
