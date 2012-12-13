package aima.core.probability

trait ProbabilityModel {
  def prior(φ: Proposition): Double
  def posterior(φ: Proposition, evidence: Set[Proposition]): Double = {
    val conjEvidence = evidence reduce {_ ∧ _}
    prior(conjEvidence) match {
      case 0 => 0
      case prob => prior(φ ∧ conjEvidence) / prob
    }
  }
  def variables: Set[RandomVariable[_]]
}
