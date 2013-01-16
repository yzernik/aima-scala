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

package aima.core

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
package object agent {
  /**
   * A simple type for a "condition-action rule" matcher
   */
  type RuleMatcher[S, A] = S => Option[A]
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): pg 50.<br>
   *
   * This knowledge about "how the world works" - whether implemented in simple
   * Boolean circuits or in complete scientific theories - is called a model of
   * the world. An Agent that uses such a model is called a model-based agent.
   */
  type Model[S, A, P] = (S, Option[A], P) => S
}
