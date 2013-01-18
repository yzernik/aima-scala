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

package aima.core.probability.bayes.approximate

import aima.core.probability.bayes.{BayesianNetwork, BayesianSampleInference}
import aima.core.probability.impl._
import aima.core.probability.{CategoricalDistribution, RandomVariable}

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 533.<br>
 * <br>
 *
 * <pre>
 * function REJECTION-SAMPLING(X, e, bn, N) returns an estimate of <b>P</b>(X|e)
 * inputs: X, the query variable
 * e, observed values for variables E
 * bn, a Bayesian network
 * N, the total number of samples to be generated
 * local variables: <b>N</b>, a vector of counts for each value of X, initially zero
 *
 * for j = 1 to N do
 * <b>x</b> <- PRIOR-SAMPLE(bn)
 * if <b>x</b> is consistent with e then
 * <b>N</b>[x] <- <b>N</b>[x] + 1 where x is the value of X in <b>x</b>
 * return NORMALIZE(<b>N</b>)
 * </pre>
 *
 * Figure 14.14 The rejection-sampling algorithm for answering queries given
 * evidence in a Bayesian Network.<br>
 * <br>
 * <b>Note:</b> The implementation has been extended to handle queries with
 * multiple variables. <br>
 *
 * @author Alex DiCarlo
 */
object rejectionSampling extends BayesianSampleInference {
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork,
    sampleCnt: Int): CategoricalDistribution = {
    def isConsistent(x: List[AssignmentProposition]): Boolean =
      evidence forall {assignment => assignment == (x find (_ == assignment) getOrElse assignment)}

    val variables = X map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("rejectionSampling only works with FiniteRandomVariables")
    }
    val variableTable = VariableTable(variables)
    val values = ((0 to sampleCnt) foldLeft IndexedSeq.fill(variableTable.size)(0.0)) {case (samples, _) =>
      val sample = priorSample(variables, evidence, network)
      val index = variableTable.index(sample)
      if (isConsistent(sample)) samples.updated(index, samples(index) + 1) else samples
    }
    ProbabilityTable(values.toList, variableTable)
  }

  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): CategoricalDistribution =
    this(X, evidence, network, 10000) // std. deviation of error is proportional to 1 / root(n) = 1 / 100
}
