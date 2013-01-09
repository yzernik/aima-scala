package aima.core.probability.bayes

import aima.core.probability.impl.{FiniteRandomVariable, AssignmentProposition}
import aima.core.probability.{Factor, CategoricalDistribution}
import scala.annotation.tailrec

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

  override def sample(probabilityChoice: Double, parentValues: List[AssignmentProposition]): A = {
    val distribution = conditioningCaseFor(parentValues).values
    require(variable.domain.finiteValues.size == distribution.length, s"Size of domain of variable: " +
      s"${variable.domain.finiteValues.size} is not equal to the size of the distribution: ${distribution.length}")
    @tailrec
    def recur(distribution: List[Double], total: Double, index: Int): Int = distribution match {
      case value :: tail if probabilityChoice > total => recur(tail, total + value, index + 1)
      case _ => index
    }
    val index = recur(distribution, 0.0, 0)
    variable.domain.indexedValues(index)
  }

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