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

package aima.core.search.nondeterministic

/**
 * Represents a solution plan for an AND-OR search; according to page 135
 * AIMA3e, the plan must be "a subtree that (1) has a goal node at every leaf,
 * (2) specifies one Object at each of its OR nodes, and (3) includes every
 * outcome branch at each of its AND nodes." As demonstrated on page 136, this
 * subtree is implemented as a linked list where every OR node is an Object--
 * satisfying (2)--and every AND node is an if-state-then-plan-else
 * chain--satisfying (3).
 *
 * <b>Note:</b> Specifically, at an OR node in the tree we choose one action which leads to a state where the
 * environment "chooses" a state for us, leading to an AndPlan that reacts to that state. A solution produced by
 * andOrGraphSearch will never throw an IllegalArgumentException, because we will never reach an invalid state
 * (otherwise the search would have failed in the first place).
 *
 * Author: dicarlo2 (Alex)
 * Date: 11/20/12
 */
final class OrPlan[S, A](state: S, action: Option[A], andPlan: Option[AndPlan[S, A]]) {
  def apply(state: S): (Option[A], Option[AndPlan[S, A]]) =
    if (state == this.state)
      (action, andPlan)
    else
      throw new IllegalArgumentException("Invalid state for OrPlan")
}
