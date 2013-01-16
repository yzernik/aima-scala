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

import aima.core.probability.impl.{FiniteRandomVariable, AssignmentProposition}
import scala.annotation.tailrec

package object bayes {
  def sample[A](
    variable: FiniteRandomVariable[A],
    probabilityChoice: Double,
    parentValues: List[AssignmentProposition],
    distribution: List[Double]): A = {
    require(variable.domain.finiteValues.size == distribution.length, s"Size of domain of variable: " +
      s"${variable.domain.finiteValues.size} is not equal to the size of the distribution: ${distribution.length}")
    @tailrec
    def recur(distribution: List[Double], total: Double, index: Int): Int = distribution match {
      case value :: tail if probabilityChoice > total => recur(tail, total + value, index + 1)
      case _ => index
    }
    val index = recur(distribution, 0.0, 0)
    variable.domain.indexedValues(index)
  }
}
