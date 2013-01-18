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
 * An abstract description of possible discrete Environments in which Agent(s)
 * can perceive and act.
 *
 * @tparam A Action
 * @tparam P Percept
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
abstract class Environment[A, P](startState: EnvironmentState) {
  /**
   * Measures performance of agent in current environment
   * @param agent to be measured
   * @return value where agents with better performance have higher value
   */
  def measurePerformance(agent: Agent[A, P]): Double

  override def equals(other: Any): Boolean

  def canEqual(other: Any): Boolean

  override def hashCode: Int
}
