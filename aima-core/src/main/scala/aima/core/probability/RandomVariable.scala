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
  val name: String
  /**
   * The domain of this RandomVariable
   */
  val domain: Domain[A]
  /**
   * The set of possible values the Random Variable can take on.
   */
  lazy val values: Stream[A] = domain.values
}
