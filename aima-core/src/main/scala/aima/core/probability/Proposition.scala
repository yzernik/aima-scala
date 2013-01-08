package aima.core.probability

import aima.core.probability.World.AnyWorld

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 486.<br>
 * <br>
 * Propositions describing sets of possible worlds are written in a notation
 * that combines elements of propositional logic and constraint satisfaction
 * notation. In the terminology of Section 2.4.7, it is a factored
 * representation, in which a possible world is represented by a set of
 * variable/value pairs.<br>
 * <br>
 * A possible world is defined to be an assignment of values to all of the
 * random variables under consideration.
 *
 * @author Alex DiCarlo
 */
trait Proposition {
  /**
   * The set of RandomVariables in the World (sample space) that this Proposition is applicable to.
   */
  val scope: Set[RandomVariable[_]]
  /**
   * Determine whether or not the proposition holds in a particular possible world.
   *
   * @param possibleWorld A possible world is defined to be an assignment of values to all of the random
   *                      variables under consideration.
   * @return true if the proposition holds in the given possible world, false otherwise.
   */
  def holdsIn(possibleWorld: AnyWorld): Boolean
}

abstract class AtomicProposition(val scope: Set[RandomVariable[_]]) extends Proposition
abstract class ComplexProposition(val propositions: Set[Proposition]) extends Proposition {
  val scope: Set[RandomVariable[_]] = propositions flatMap {_.scope}
}