/*
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

package aima.core.probability

import aima.core.probability.impl._
import aima.core.probability.impl.FiniteSingleWorld
import aima.core.probability.impl.AssignmentProposition

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 487.<br>
 * <br>
 * A probability distribution for discrete random variables with a finite set of
 * values. <br>
 * <b>Note:</b> This definition corresponds to that given in AIMA3e pg. 487, for
 * a Probability Distribution.
 *
 * @see <a href="http://en.wikipedia.org/wiki/Probability_distribution"
 *      >Probability Distribution</a>
 *
 * @author Alex DiCarlo
 */
trait CategoricalDistribution extends ProbabilityMass with Traversable[(FiniteSingleWorld, Double)] {
  def values: List[Double]
  override def variables: List[FiniteRandomVariable[_]]
  /**
   * Retrieve the index into the CategoricalDistribution for the provided set
   * of values for the random variables comprising the Distribution.
   *
   * @param assignments an ordered set of values for the random variables comprising
   *            the Distribution (<b>Note:</b> the order must match the order
   *            of the random variables describing the distribution)
   * @return the index within the Distribution for the values specified.
   *
   * @see ProbabilityDistribution#value()
   * @see CategoricalDistribution#values
   * @see ProbabilityDistribution#getFor()
   */
  def index(assignments: List[AssignmentProposition]): Int

  def value(assignments: List[AssignmentProposition]): Double =  values(index(assignments))

  /**
   * Normalize the values comprising this distribution.
   *
   * @return CategoricalDistribution with normalized values
   */
  def normalize(): CategoricalDistribution

  /**
   * Get the marginal probability for the provided variables from this
   * Distribution creating a new Distribution of the remaining variables with
   * their values updated with the summed out random variables.<br>
   * <br>
   * see: AIMA3e page 492.<br>
   * <br>
   *
   * @param vars the random variables to marginalize/sum out.
   * @return a new Distribution containing any remaining random variables not
   *         summed out and a new set of values updated with the summed out
   *         values.
   */
  def marginal(vars: Set[FiniteRandomVariable[_]]): CategoricalDistribution

  /**
   * Divide the dividend (this) CategoricalDistribution by the divisor to
   * create a new CategoricalDistribution representing the quotient. The
   * variables comprising the divisor distribution must be a subset of the
   * dividend. However, ordering of variables does not matter as the quotient
   * contains the same variables as the dividend and the internal
   * implementation logic should handle iterating through the two
   * distributions correctly, irrespective of the order of their variables.
   *
   * @param divisor
   * @return a new Distribution representing the quotient of the dividend
   *         (this) divided by the divisor.
   * @throws IllegalArgumentException if the variables of the divisor distribution
   *                                  are not a subset of the dividend.
   */
  def divideBy(divisor: CategoricalDistribution): CategoricalDistribution

  /**
   * Multiplication of this Distribution by a given multiplier, creating a new
   * Distribution representing the product of the two. <b>Note:</b> Is
   * equivalent to pointwise product calculation on factors.<br>
   * <br>
   * see: AIMA3e Figure 14.10 page 527.<br>
   * <br>
   * Note: Default Distribution multiplication is not commutative. The reason
   * is because the order of the variables comprising a Distribution dictate
   * the ordering of the values for that distribution. For example (the
   * General case of Baye's rule, AIMA3e pg. 496), using this API method:<br>
   * <br>
   * <b>P</b>(Y | X) = (<b>P</b>(X | Y)<b>P</b>(Y))/<b>P</b>(X)<br>
   * <br>
   * is NOT true, due to multiplication of distributions not being
   * commutative. However:<br>
   * <br>
   * <b>P</b>(Y | X) = (<b>P</b>(Y)<b>P</b>(X | Y))/<b>P</b>(X)<br>
   * <br>
   * is true, using this API.<br>
   * <br>
   * The default order of the variable of the Distribution returned is the
   * order of the variables as they are seen, as read from the left to right
   * term, for e.g.: <br>
   * <br>
   * <b>P</b>(Y)<b>P</b>(X | Y)<br>
   * <br>
   * would give a Distribution of the following form: <br>
   * Y, X<br>
   * <br>
   * i.e. an ordered union of the variables from the two distributions. <br>
   * To override the default order of the product use multiplyByPOS().
   *
   * @param multiplier
   *
   * @return a new Distribution representing the product of this and the
   *         passed in multiplier. The order of the variables comprising the
   *         product distribution is the ordered union of the left term (this)
   *         and the right term (multiplier).
   *
   * @see CategoricalDistribution#multiplyByPOS(CategoricalDistribution, Set[RandomVariable])
   */
  def multiplyBy(multiplier: CategoricalDistribution): CategoricalDistribution =
    multiplyByPOS(multiplier, (variables ::: multiplier.variables).distinct)

  /**
   * Multiplication - Product Order Specified (POS). <b>Note:</b> Is
   * equivalent to pointwise product calculation.<br>
   * <br>
   * see: AIMA3e Figure 14.10 page 527.<br>
   * <br>
   * Multiplication of this Distribution by a given multiplier, creating a new
   * Distribution representing the product of the two. The order of the
   * variables comprising the product will match those specified. For example
   * (the General case of Baye's rule, AIMA3e pg. 496), using this API method:<br>
   * <br>
   * <b>P</b>(Y | X) = (<b>P</b>(X | Y)<b>P</b>(Y), [Y, X])/<b>P</b>(X)<br>
   * <br>
   * is true when the correct product order is specified.
   *
   * @param multiplier
   * @param prodVarOrder
         * the order the variables comprising the product are to be in.
   *
   * @return a new Distribution representing the product of this and the
   *         passed in multiplier. The order of the variables comprising the
   *         product distribution is the order specified.
   *
   * @see CategoricalDistribution#multiplyBy(CategoricalDistribution)
   */
  def multiplyByPOS(multiplier: CategoricalDistribution, prodVarOrder: List[FiniteRandomVariable[_]]): CategoricalDistribution
}

