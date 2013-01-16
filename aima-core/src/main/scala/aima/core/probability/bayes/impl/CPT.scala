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
import aima.core.probability.bayes.ConditionalProbabilityTable
import aima.core.probability.impl._
import aima.core.util.MathUtils._
import scala.annotation.tailrec

/**
 * Default implementation of the ConditionalProbabilityTable interface.
 *
 * @author Alex DiCarlo
 */
final class CPT[A](
  val variable: FiniteRandomVariable[A],
  values: List[Double],
  val parents: List[FiniteRandomVariable[_]]) extends ConditionalProbabilityTable[A] {
  private val table: ProbabilityTable = ProbabilityTable(values, VariableTable(parents :+ variable))
  require(eachRowTotalsOne(), "A row of the CPT did not total to 1.0")

  def value(assignments: List[AssignmentProposition]): Double = table.value(assignments)

  def conditioningCaseFor(parentValues: List[AssignmentProposition]): CategoricalDistribution = {
    require(parentValues.length == parents.length, s"The number of parent value arguments [${parentValues.length}]" +
      s"is not equal to the number of parents [${parents.length}] for this CPT")
    val conditioningCase = table filter {case (world, _) =>
      parentValues forall {case AssignmentProposition(assign) => world.assignments.valuesIterator contains assign}
    }
    val values = conditioningCase map {_._2}
    ProbabilityTable(values.toList, VariableTable(List(variable))).normalize()
  }

  def factorFor(evidence: List[AssignmentProposition]): Factor = {
    val freeVariables = variables diff (evidence map {_.assign.variable})
    table sumOut freeVariables.toSet
  }

  private def eachRowTotalsOne(): Boolean = {
    @tailrec
    def recur(probabilities: List[(FiniteSingleWorld, Double)], cnt: Int, rowProb: Double): Boolean =
      probabilities match {
        case _ :: tail if cnt % variable.finiteValues.size == 0 && (rowProb ~= 1.0) => false
        case (_, prob) :: tail if cnt % variable.finiteValues.size == 0 => recur(tail, cnt + 1, prob)
        case (_, prob) :: tail => recur(tail, cnt + 1, rowProb + prob)
        case Nil => true
      }
    recur(table.toList, 0, 1.0)
  }
}