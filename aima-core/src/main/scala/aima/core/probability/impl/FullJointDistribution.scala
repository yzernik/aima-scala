package aima.core.probability.impl

import aima.core.probability._
import aima.core.probability.impl.Proposition.ProbabilityProposition
import aima.core.probability.World.world2AnyWorld
import aima.core.util.MathUtils._
import scala.annotation.tailrec

/**
 * An implementation of the FiniteProbabilityModel API using a full joint
 * distribution as the underlying model.
 *
 * @author Alex DiCarlo
 */
final class FullJointDistribution(values: List[Double], vars: List[FiniteRandomVariable[_]]) extends FiniteProbabilityModel {
  private val distribution: ProbabilityTable = ProbabilityTable(values, VariableTable(vars))

  val variables: List[RandomVariable[_]] = vars

  def isValid: Boolean = within(0.0, distribution.sum, 1.0)

  def prior(φ: Proposition): Double = (for {(world, prob) <- distribution if φ holdsIn world} yield prob).sum

  def jointDistribution(propositions: Proposition*): CategoricalDistribution = {
    val conjProp = propositions reduce {_ ∧ _}
    val unbound = (variables diff conjProp.scope.toList) map {
      case variable: FiniteRandomVariable[_] => variable
      case _ => throw new IllegalArgumentException("Scope of proposition must contain only FiniteRandomVariables")
    }
    val unboundVarTable = VariableTable(unbound)
    @tailrec
    def recur(worldProbs: List[(FiniteSingleWorld, Double)], unboundValues: IndexedSeq[Double]): List[Double] =
      worldProbs match {
        case (world, prob) :: tail =>
          val assignment = for (rv <- unbound) yield AssignmentProposition(world(rv))
          val distribIdx = unboundVarTable.index(assignment)
          recur(tail, unboundValues.updated(distribIdx, unboundValues(distribIdx) + prob))
        case Nil => unboundValues.toList
      }
    val unboundValues = recur(distribution.toList, IndexedSeq.fill(unboundVarTable.size)(0.0))
    ProbabilityTable(unboundValues, unboundVarTable)
  }
}