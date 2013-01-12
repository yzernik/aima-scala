package aima.core.probability.bayes

import aima.core.probability.CategoricalDistribution
import aima.core.probability.impl.AssignmentProposition
import aima.core.probability.RandomVariable

/**
 * General interface to be implemented by approximate Bayesian Inference
 * algorithms.
 *
 * @author Alex DiCarlo
 */
trait BayesianSampleInference extends BayesianInference {
  /**
   * @param X the query variables.
   * @param evidence observed values for variables E.
   * @param network a Bayes net with variables {X} &cup; E &cup; Y / Y = hidden variables
   * @param samples number of samples to take
   * @return a distribution over the query variables.
   */
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork,
    samples: Int): CategoricalDistribution
}
