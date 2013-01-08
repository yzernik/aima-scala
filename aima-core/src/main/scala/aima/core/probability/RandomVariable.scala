package aima.core.probability

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 486.<br>
 * <br>
 * Variables in probability theory are called random variables and their names
 * begin with an uppercase letter. Every random variable has a domain - the set
 * of possible values it can take on.
 *
 * @author Alex DiCarlo
 */
trait RandomVariable[A] {
  /**
   * The number used to uniquely identify this variable.
   */
  val num: Int
  /**
   * The domain of this RandomVariable
   */
  val domain: Domain[A]
  /**
   * The set of possible values the Random Variable can take on.
   */
  lazy val values: Stream[A] = domain.values
}
