package aima.core.probability.bayes.approximate

import aima.core.probability.RandomVariable
import aima.core.probability.bayes.BayesianNetwork
import aima.core.probability.impl._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 531.<br>
 * <br>
 *
 * <pre>
 * function PRIOR-SAMPLE(bn) returns an event sampled from the prior specified by bn
 *   inputs: bn, a Bayesian network specifying joint distribution <b>P</b>(X<sub>1</sub>,...,X<sub>n</sub>)
 *
 *   x <- an event with n elements
 *   foreach variable X<sub>i</sub> in X<sub>1</sub>,...,X<sub>n</sub> do
 *      x[i] <- a random sample from <b>P</b>(X<sub>i</sub> | parents(X<sub>i</sub>))
 *   return x
 * </pre>
 *
 * Figure 14.13 A sampling algorithm that generates events from a Bayesian
 * network. Each variable is sampled according to the conditional distribution
 * given the values already sampled for the variable's parents.
 *
 * @author Alex DiCarlo
 */
object priorSample {
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): List[AssignmentProposition] = {
    val networkVariables = network.variables map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("priorSample only works with FiniteRandomVariables")
    }
    val sample = (networkVariables foldLeft Map[RandomVariable[_], AssignmentProposition]()) {
      case (assignments, variable) =>
        (network nodeFor variable) map {node =>
          val parentAssignments = (assignments filter {case (v, assign) => node.parents.contains(v)}).values.toList
          val sample = node.cpd.sample(util.Random.nextDouble(), parentAssignments)
          val assignment = AssignmentProposition(SingleAssignment(variable, sample))
          assignments + (variable -> assignment)
        } getOrElse assignments
    }
    sample.values.to[List]
  }
}
