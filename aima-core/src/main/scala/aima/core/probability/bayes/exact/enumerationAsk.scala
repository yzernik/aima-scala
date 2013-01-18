/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
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

package aima.core.probability.bayes.exact

import aima.core.probability.bayes._
import aima.core.probability.impl.ProbabilityTable.createPossibleWorlds
import aima.core.probability.impl._
import aima.core.probability.{CategoricalDistribution, RandomVariable}

object enumerationAsk extends BayesianInference {
  /**
   * @param X the query variables.
   * @param evidence observed values for variables E.
   * @param network a Bayes net with variables {X} &cup; E &cup; Y / Y = hidden variables
   * @return a distribution over the query variables.
   */
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): CategoricalDistribution = {
    def enumerateAll(vars: List[RandomVariable[_]], evidence: List[AssignmentProposition]): Double = vars match {
      case variable :: tail if evidence exists {_.assign.variable == variable} =>
        posteriorForParents(variable, evidence) * enumerateAll(tail, evidence)
      case variable :: tail =>
        def sum[A](variable: RandomVariable[A]): Double = {
          val values = for {y <- variable.domain.values
            extendedEvidence = evidence :+ AssignmentProposition(SingleAssignment(variable, y))
          } yield posteriorForParents(variable, extendedEvidence) * enumerateAll(tail, extendedEvidence)
          values.sum
        }
        sum(variable)
      case Nil => 1.0
    }
    def posteriorForParents[A](variable: RandomVariable[A], evidence: List[AssignmentProposition]): Double = {
      (network nodeFor variable) match {
        case node: FiniteNode[A] =>
          node.cpt value (evidence filter { prop => (node.parents :+ variable) exists {_ == prop.assign.variable}})
        case _ => throw new IllegalArgumentException("Enumeration-Ask only works with finite Nodes.")
      }
    }

    val variables = X map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("Enumeration-Ask only works with FiniteRandomVariables")
    }
    val Q = VariableTable(variables)
    val Qvalues =
      (createPossibleWorlds(Q.radices, Q.varInfos) foldLeft List.fill[Double](Q.size)(0.0)) { case (values, world) =>
        val assignments = (world.assignments map { case (_, assign) => AssignmentProposition(assign)})
        val probability = enumerateAll(network.variables, evidence ++ assignments)
        values.updated(Q.index(assignments.toList), probability)
      }
    ProbabilityTable(Qvalues, Q)
  }
}
