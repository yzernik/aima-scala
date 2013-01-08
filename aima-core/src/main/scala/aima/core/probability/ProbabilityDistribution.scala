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
