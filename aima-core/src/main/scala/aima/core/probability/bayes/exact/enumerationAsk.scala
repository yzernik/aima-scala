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
