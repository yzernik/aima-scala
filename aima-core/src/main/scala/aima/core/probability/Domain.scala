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
