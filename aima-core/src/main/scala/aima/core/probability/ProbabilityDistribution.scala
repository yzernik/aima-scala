package aima.core.probability

trait ProbabilityDistribution {
  def variables: Set[RandomVariable[_]]
  def value(assignmentPropositions: Set[AssignmentProposition]): Option[Double]
}
