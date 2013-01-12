package aima.core.probability.bayes.approximate

import aima.core.probability.bayes._
import aima.core.probability.impl._
import aima.core.probability.{CategoricalDistribution, RandomVariable}
import scala.Some

object gibbsAsk extends BayesianSampleInference {
  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork,
    samples: Int): CategoricalDistribution = {
    val nonEvidenceVars = network.variables filter {variable => evidence exists {_.assign.variable == variable}}
    val evidenceMap = (evidence map {assign => (assign.assign.variable → assign)}).toMap
    val initialState = (nonEvidenceVars foldLeft evidenceMap) {case (event, variable) =>
      val sample = (network nodeFor variable).map(node => randomSample(node, event.values.to[List])).get
      event + (variable → AssignmentProposition(SingleAssignment(variable, sample)))
    } map {case (variable, assign) => assign}
    val variables = X map {
      case x: FiniteRandomVariable[_] => x
      case _ => throw new IllegalArgumentException("rejectionSampling only works with FiniteRandomVariables")
    }
    val variableTable = VariableTable(variables)
    val (_, values) = ((0 to samples) foldLeft (initialState.to[List], IndexedSeq.fill(variableTable.size)(0.0))) {
      case ((state, values), _) =>
        val newState = (state foldLeft state) {
          case (currentState, AssignmentProposition(assign)) => network nodeFor assign.variable match {
            case Some(x: FiniteNode[_]) =>
              val value = markovBlanketRandomSample(x, currentState)
              AssignmentProposition(SingleAssignment(x.variable, value)) :: currentState
            case Some(_) => throw new IllegalArgumentException("gibbsAsk only works for FiniteNodes")
            case _ => throw new IllegalArgumentException(s"${assign.variable} was not in the bayesian network")
          }
        }
        val index = variableTable.index(newState)
        (newState, values.updated(index, values(index) + 1))
    }
    ProbabilityTable(values.to[List], variableTable).normalize()
  }

  def apply(
    X: List[RandomVariable[_]],
    evidence: List[AssignmentProposition],
    network: BayesianNetwork): CategoricalDistribution = this(X, evidence, network, 1000)
}
