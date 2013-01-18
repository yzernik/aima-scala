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

import aima.core.probability._
import aima.core.probability.bayes._
import aima.core.probability.impl.{FiniteRandomVariable, AssignmentProposition}
import scala.Some

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 14.11, page
 * 528.<br>
 * <br>
 *
 * <pre>
 * function ELIMINATION-ASK(X, e, bn) returns a distribution over X
 *   inputs: X, the query variable
 *           e, observed values for variables E
 *           bn, a Bayesian network specifying joint distribution P(X<sub>1</sub>, ..., X<sub>n</sub>)
 *
 *   factors <- []
 *   for each var in ORDER(bn.VARS) do
 *       factors <- [MAKE-FACTOR(var, e) | factors]
 *       if var is hidden variable the factors <- SUM-OUT(var, factors)
 *   return NORMALIZE(POINTWISE-PRODUCT(factors))
 * </pre>
 *
 * Figure 14.11 The variable elimination algorithm for inference in Bayesian
 * networks. <br>
 * <br>
 * <b>Note:</b> The implementation has been extended to handle queries with
 * multiple variables. <br>
 *
 * @author Alex DiCarlo
 */
object eliminationAsk extends BayesianInference {
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): CategoricalDistribution = {
    def makeFactor(variable: RandomVariable[_]): Factor = network nodeFor variable match {
      case Some(x: FiniteNode[_]) => x.cpt factorFor (evidence filter x.cpt.variables.contains)
      case _ => throw new IllegalArgumentException("Elimination-Ask only works with finite Nodes.")
    }

    def sumOut(variable: FiniteRandomVariable[_], factors: List[Factor]): List[Factor] = {
      val grouped = factors groupBy {_.variables.contains(variable)} withDefaultValue List()
      (grouped(true) reduce {(f1, f2) => f1 pointwiseProduct f2} sumOut Set(variable)) :: grouped(false)
    }

    val networkVariables = network.variables map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("Elimination-Ask only works with FiniteRandomVariables")
    }
    val hidden = networkVariables filter
      {variable => (X contains variable) || (evidence exists {_.assign.variable == variable})}
    val factors = (networkVariables foldLeft List[Factor]()) {
      case (factors, variable) if (hidden contains variable) => sumOut(variable, makeFactor(variable) :: factors)
      case (factors, variable) => makeFactor(variable) :: factors
    }
    factors reduce {(f1, f2) => f1 pointwiseProduct f2} normalize ()
  }
}
