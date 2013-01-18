/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
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

package aima.core.probability.bayes

import aima.core.probability.{ProbabilityDistribution, RandomVariable}
import aima.core.probability.impl.AssignmentProposition

/**
* A conditional probability distribution on a RandomVariable X<sub>i</sub>:<br>
* <br>
* P(X<sub>i</sub> | Parents(X<sub>i</sub>)) that quantifies the effect of the parents on X<sub>i</sub>.
*
* @author Alex DiCarlo
*/
trait ConditionalProbabilityDistribution[A] extends ProbabilityDistribution {
  /**
  * the Random Variable this conditional probability distribution is on.
  */
  def variable: RandomVariable[A]

  /**
  * a consistent ordered Set (e.g. LinkedHashSet) of the parent Random Variables for this conditional distribution.
  */
  def parents: List[RandomVariable[_]]

  /**
   * A convenience method for merging the elements of 'parents' and 'variable' into a consistent ordered set
   * (i.e. 'variable' should always be the last Random Variable returned when iterating over the set).
   *
   * a consistent ordered Set of the random variables this conditional probability distribution is for.
   */
  lazy val variables: List[RandomVariable[_]] = (parents :+ variable).distinct

  /**
  * A conditioning case is just a possible combination of values for the
  * parent nodes - a miniature possible world, if you like. The returned
  * distribution must sum to 1, because the entries represent an exhaustive
  * set of cases for the random variable.
  *
  * @param parentValues for the conditioning case. The ordering and size of parentValues must equal parents and
   *                     their domains must match.
  * @return the Probability Distribution for the Random Variable the Conditional Probability Distribution is On.
  * @see ConditionalProbabilityDistribution#variable
  * @see ConditionalProbabilityDistribution#parents
  */
  def conditioningCaseFor(parentValues: List[AssignmentProposition]): ProbabilityDistribution

  /**
  * Retrieve a specific example for the Random Variable this conditional distribution is on using the given
   * probabilityChoice value.
  *
  * @param probabilityChoice a double value, from the range [0.0d, 1.0d), i.e. 0.0d (inclusive) to 1.0d (exclusive).
  * @param parentValues for the conditioning case. The size of parentValues must equal parents and their Random
   *                     Variables must match.
  * @return a sample value from the domain of the Random Variable this distribution is on,
   *         based on the probability argument passed in.
  * @see ConditionalProbabilityDistribution#variable
  */
  def sample(probabilityChoice: Double, parentValues: List[AssignmentProposition]): A
}