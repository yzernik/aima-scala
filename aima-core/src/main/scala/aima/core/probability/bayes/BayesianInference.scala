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
