package aima.core.probability

import aima.core.probability.impl.Proposition.ProbabilityProposition

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 484.<br>
 * <br>
 * A fully specified probability model associates a numerical probability
 * P(&omega;) with each possible world. The set of all possible worlds is called
 * the sample space &Omega;.
 *
 * @author Alex DiCarlo
 */
trait ProbabilityModel {
  /**
   * a consistent ordered Set of the random variables describing the atomic variable/value pairs this probability model
   * can take on. Refer to pg. 486 AIMA3e.
   */
  def variables: List[RandomVariable[_]]
  /**
   * true, if 0 <= P(&omega;) <= 1 for every &omega; and &sum;<sub>&omega; &isin; &Omega;</sub> P(&omega;) = 1
   * (Equation 13.1 pg. 484 AIMA3e), false otherwise.
   */
  def isValid: Boolean
  /**
   * For any proposition &phi;, P(&phi;) = &sum;<sub>&omega; &isin;
   * &phi;</sub> P(&omega;). Refer to equation 13.2 page 485 of AIMA3e.
   * Probabilities such as P(Total = 11) and P(doubles) are called
   * unconditional or prior probabilities (and sometimes just "priors" for
   * short); they refer to degrees of belief in propositions in the absence of
   * any other information.
   *
   * @param φ the propositional terms for which a probability value is to be returned.
   * @return the probability of the proposition &phi;.
   */
  def prior(φ: Proposition*): Double
  /**
   * Unlike unconditional or prior probabilities, most of the time we have
   * some information, usually called evidence, that has already been
   * revealed. This is the conditional or posterior probability (or just
   * "posterior" for short). Mathematically speaking, conditional
   * probabilities are defined in terms of unconditional probabilities as
   * follows, for any propositions a and b, we have:<br>
   * <br>
   * P(a | b) = P(a AND b)/P(b)<br>
   * <br>
   * which holds whenever P(b) > 0. Refer to equation 13.3 page 485 of AIMA3e.
   * This can be rewritten in a different form called the <b>product rule</b>: <br>
   * <br>
   * P(a AND b) = P(a | b)P(b)<br>
   * <br>
   * and also as:<br>
   * <br>
   * P(a AND b) = P(b | a)P(a)<br>
   * <br>
   * whereby, equating the two right-hand sides and dividing by P(a) gives you
   * Bayes' rule:<br>
   * <br>
   * P(b | a) = (P(a | b)P(b))/P(a) - i.e. (likelihood * prior)/evidence
   *
   * @param φ the proposition for which a probability value is to be
   *          returned.
   * @param evidence information we already have.
   * @return the probability of the proposition &phi; given evidence.
   */
  def posterior(φ: Proposition, evidence: Proposition*): Double = {
    val conjEvidence = evidence reduce {_ ∧ _}
    prior(conjEvidence) match {
      case 0 => 0
      case prob => prior(φ ∧ conjEvidence) / prob
    }
  }
}
