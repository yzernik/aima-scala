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

package aima.core.probability.bayes.impl

import aima.core.probability._
import aima.core.probability.impl.Proposition.{variableToTermProposition, ProbabilityProposition}
import aima.core.probability.bayes.{BayesianInference, BayesianNetwork}
import aima.core.util.MathUtils._
import aima.core.probability.impl.FullJointDistribution.jointDistrib

/**
 * Very simple implementation of the FiniteProbabilityModel API using a Bayesian
 * Network, consisting of FiniteNodes, to represent the underlying model.<br>
 * <br>
 * <b>Note:</b> The implementation currently doesn't take advantage of the use
 * of evidence values when calculating posterior values using the provided
 * Bayesian Inference implementation (e.g enumerationAsk). Instead it simply
 * creates a joint distribution over the scope of the propositions (i.e. using
 * the inference implementation with just query variables corresponding to the
 * scope of the propositions) and then iterates over this to get the appropriate
 * probability values. A smarter version, in the general case, would need to
 * dynamically create deterministic nodes to represent the outcome of logical
 * and derived propositions (e.g. conjuncts and summations over variables) in
 * order to be able to correctly calculate using evidence values.
 *
 * @author Alex DiCarlo
 */
final class FiniteBayesModel(private val network: BayesianNetwork, private val bayesInference: BayesianInference)
  extends FiniteProbabilityModel {
  def variables: List[RandomVariable[_]] = network.variables

  def isValid: Boolean = prior((variables map variableToTermProposition): _*)  ~= 1

  def prior(φ: Proposition*): Double = {
    val conjProp = φ reduce {_ ∧ _}
    val distribution = bayesInference(conjProp.scope.to[List], List(), network)
    (distribution foldLeft 0.0){case (sum, (world, prob)) => if (conjProp holdsIn world) sum + prob else sum}
  }

  override def posteriorDistribution(φ: Proposition, evidence: Proposition*): CategoricalDistribution = {
    val distribution = super.posteriorDistribution(φ, evidence: _*)
    // Note: Need to ensure normalize() is called in order to handle the case where an approximate
    // algorithm is used (i.e. won't evenly divide as will have calculated on separate approximate
    // runs). However, this should only be done if the all of the evidences scope are bound (if not
    // you are returning in essence a set of conditional distributions, which you do not want normalized).
    val unboundEvidence = evidence exists {_.isInstanceOf[TermProposition]}
    if (unboundEvidence) distribution else distribution.normalize()
  }

  def jointDistribution(φ: Proposition*): CategoricalDistribution = {
    val conjProp = φ reduce {_ ∧ _}
    val distribution = bayesInference(conjProp.scope.to[List], List(), network)
    jointDistrib(distribution, variables, φ: _*)
  }
}
