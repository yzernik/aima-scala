package aima.example.probability.bayes

import aima.core.probability.bayes.BayesianNetwork
import aima.core.probability.bayes.impl.{BayesNet, CPTNode}

object ToothCavityCatchNetwork {
  def apply(): BayesianNetwork = {
    lazy val cavity: CPTNode[Boolean] = CPTNode(RandomVariables.cavityRV, List(0.2, 0.8), List(), List(toothache,
      catchNode))
    lazy val toothache: CPTNode[Boolean] = CPTNode(RandomVariables.toothacheRV, List(
      // C=true, T=true
      0.6,
      // C=true, T=false
      0.4,
      // C=false, T=true
      0.1,
      // C=false, T=false
      0.9)
      , List[CPTNode[Boolean]](cavity), List())
    lazy val catchNode: CPTNode[Boolean] = CPTNode(RandomVariables.catchRV, List(
      // C=true, Catch=true
      0.9,
      // C=true, Catch=false
      0.1,
      // C=false, Catch=true
      0.2,
      // C=false, Catch=false
      0.8
    ), List(cavity), List())
    BayesNet(cavity)
  }
}