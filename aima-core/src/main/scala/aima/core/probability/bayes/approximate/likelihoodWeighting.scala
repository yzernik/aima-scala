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

package aima.core.probability.bayes.approximate

import aima.core.probability.bayes.{BayesianNetwork, BayesianSampleInference}
import aima.core.probability.impl._
import aima.core.probability.{CategoricalDistribution, RandomVariable}
import aima.core.probability.impl.FiniteRandomVariable
import aima.core.probability.impl.AssignmentProposition

object likelihoodWeighting extends BayesianSampleInference {
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork,
    samples: Int): CategoricalDistribution = {
    val networkVariables = network.variables map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("priorSample only works with FiniteRandomVariables")
    }
    def weightedSample(): (List[AssignmentProposition], Double) = {
      (networkVariables foldLeft (List[AssignmentProposition](), 1.0)) {
        case ((assignments, weight), variable) if evidence exists {_.assign.variable == variable} =>
          network nodeFor variable map {node =>
            val parentAssignments = (assignments filter {assign => node.parents.contains(assign.assign.variable)})
            (assignments, weight * (node.cpd value parentAssignments))
          } getOrElse (assignments, weight)
        case ((assignments, weight), variable) =>
          (network nodeFor variable) map {node =>
            val parentAssignments = assignments filter {
              case AssignmentProposition(SingleAssignment(v, _)) => node.parents.contains( v)
            }
            val sample = node.cpd.sample(util.Random.nextDouble(), parentAssignments)
            (AssignmentProposition(SingleAssignment(variable, sample)) :: assignments, weight)
          } getOrElse (assignments, weight)
      }
    }

    val variables = X map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("rejectionSampling only works with FiniteRandomVariables")
    }
    val variableTable = VariableTable(variables)
    val values = ((0 to samples) foldLeft IndexedSeq.fill(variableTable.size)(0.0)) {case (samples, _) =>
      val (sample, weight) = weightedSample()
      val index = variableTable.index(sample)
      samples.updated(index, samples(index) + weight)
    }
    ProbabilityTable(values.toList, variableTable)
  }

  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): CategoricalDistribution =
    this(X, evidence, network, 10000)
}
