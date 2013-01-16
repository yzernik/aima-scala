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
