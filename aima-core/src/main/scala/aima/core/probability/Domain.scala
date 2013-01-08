package aima.core.probability

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 486.<br>
 * <br>
 * Every random variable has a <b>domain</b> - the set of possible values it can
 * take on. The domain of <i>Total</i> for two dice is the set {2,...,12} and
 * the domain of Die<sub>1</sub> is {1,...,6}. A Boolean random variable has the
 * domain {true, false}.
 *
 * @author Ciaran O'Reilly
 */
trait Domain[A] {
  def values: Stream[A]
  /**
   * true if the domain is ordered, false otherwise. i.e. you can specify 1 object from the domain is
   * < or = another object in the domain.
   */
  val isOrdered: Boolean = false
}

trait OrderedDomain[A <: Ordered[A]] {self: Domain[A] =>
  override val isOrdered: Boolean = true
}

/**
 * A Domain over a continuous not countable set of objects (e.g. the Real numbers).
 */
trait ContinuousDomain[A] extends Domain[A]

/**
 * A Domain over a countable/discrete set of objects (may be infinite).
 */
trait DiscreteDomain[A] extends Domain[A]
