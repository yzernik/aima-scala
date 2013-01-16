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

package aima.core.probability.impl

import aima.core.probability._
import aima.core.probability.impl.Proposition.ProbabilityProposition
import aima.core.probability.impl.FullJointDistribution.jointDistrib
import aima.core.util.MathUtils._
import scala.annotation.tailrec

/**
 * An implementation of the FiniteProbabilityModel API using a full joint
 * distribution as the underlying model.
 *
 * @author Alex DiCarlo
 */
final class FullJointDistribution(values: List[Double], vars: List[FiniteRandomVariable[_]])
  extends FiniteProbabilityModel {
  private val distribution: CategoricalDistribution = ProbabilityTable(values, VariableTable(vars)).normalize()

  val variables: List[RandomVariable[_]] = vars

  def isValid: Boolean = distribution.values.sum ~= 1.0

  def prior(φ: Proposition*): Double =
    (for {(world, prob) <- distribution if φ forall {_ holdsIn world}} yield prob).sum

  def jointDistribution(φ: Proposition*): CategoricalDistribution =
    jointDistrib(distribution, variables, φ: _*)
}

object FullJointDistribution {
  def jointDistrib(
    distribution: CategoricalDistribution,
    variables: List[RandomVariable[_]],
    φ: Proposition*): CategoricalDistribution = {
    val conjProp = φ reduce {_ ∧ _}
    val unboundPropVariables = φ.to[List].flatMap[FiniteRandomVariable[_], List[FiniteRandomVariable[_]]]({
      case TermProposition(x: FiniteRandomVariable[_]) => Some(x)
      case _ => None
    })
    val unbound = unboundPropVariables ++ (variables diff conjProp.scope.to[List] map {
      case variable: FiniteRandomVariable[_] => variable
      case _ => throw new IllegalArgumentException("Scope of proposition must contain only FiniteRandomVariables")
    })

    val unboundVarTable = VariableTable(unbound)
    @tailrec
    def recur(worldProbs: List[(FiniteSingleWorld, Double)], unboundValues: IndexedSeq[Double]): List[Double] =
      worldProbs match {
        case (world, prob) :: tail if conjProp holdsIn world =>
          val assignment = for (rv <- unbound) yield AssignmentProposition(world(rv))
          val distribIdx = unboundVarTable.index(assignment)
          recur(tail, unboundValues.updated(distribIdx, unboundValues(distribIdx) + prob))
        case _ :: tail => recur(tail, unboundValues)
        case Nil => unboundValues.toList
      }
    val unboundValues = recur(distribution.toList, IndexedSeq.fill(unboundVarTable.size)(0.0))
    ProbabilityTable(unboundValues, unboundVarTable)
  }
}