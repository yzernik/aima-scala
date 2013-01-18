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

package aima.core.search.nondeterministic

/**
 * Represents an if-state-then-plan statement for use with AND-OR search;
 * explanation given on page 135 of AIMA3e.
 *
 * <b>Note:</b> Specifically, at an AND node in the tree, the state is chosen for the agent by the environment so it
 * must choose a contingency plan, an OrPlan, that reacts to the state and chooses an action. Similar to an OrPlan,
 * on a successful result from andOrGraphSearch, an IllegalArgumentException will never be thrown.
 *
 * Author: dicarlo2 (Alex)
 * Date: 11/20/12
 */
final class AndPlan[S, A](stateTable: Map[S, OrPlan[S, A]]) {
  def apply(state: S): OrPlan[S, A] =
    stateTable.getOrElse(state, throw new IllegalArgumentException("Invalid state for AndPlan"))
}
