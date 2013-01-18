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

package aima.core.framework

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.1, page 35.<br>
 *
 * Figure 2.1 Agents interact with environments through sensors and actuators.
 *
 * @tparam A Action
 * @tparam P Percept
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
trait Agent[A, P] {
  /**
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A]
}
