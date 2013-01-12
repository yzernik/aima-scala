package aima.core.probability.bayes.exact

import aima.core.probability._
import aima.core.probability.bayes._
import aima.core.probability.impl.{FiniteRandomVariable, AssignmentProposition}
import scala.Some

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
