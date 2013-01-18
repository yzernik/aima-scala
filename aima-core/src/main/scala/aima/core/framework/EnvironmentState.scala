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
 * An interface used to indicate a possible state of an Environment.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
trait EnvironmentState extends Iterator[EnvironmentState] {
  /**
   * Moves this environment one time step forward
   * @return resulting Environment
   */
  override def next(): EnvironmentState

  /**
   * Returns false if all environment tasks are complete
   * @return false if all environment tasks are complete
   */
  override def hasNext: Boolean

  /**
   * Compares this environment state with another. If they are equal, then any resulting environment states from
   * stepping (calls to #next) will be equal unless the environment is stochastic. If it is stochastic,
   * they will be equal if they use the same seed.
   *
   * @param other environment
   * @return true if deterministic and equal or stochastic and same seed
   */
  override def equals(other: Any): Boolean

  def canEqual(other: Any): Boolean

  override def hashCode: Int
}
