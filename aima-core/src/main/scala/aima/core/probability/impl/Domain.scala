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

package aima.core.probability.impl

import aima.core.probability.DiscreteDomain

/**
 * A Domain over a countable/discrete and finite set of objects.
 */
case class FiniteDomain[A](finiteValues: Set[A]) extends DiscreteDomain[A] {
  lazy val values: Stream[A] = finiteValues.to[Stream]
  /**
   * a consistent ordered indexed seq of the possible values this domain can take on.
   */
  lazy val indexedValues: IndexedSeq[A] = finiteValues.to[IndexedSeq]
  /**
   * The possible values for a finite domain are to have a consistent ordering
   * (whether they are actually ordered by value or not). This will return an
   * offset into that ordering.
   *
   * @param value a value from the domain.
   * @return an offset (starting from 0) into the consistent order of the set of possible values.
   * @throws IllegalArgumentException if the value is not valid for the domain.
   */
  def offset(value: A): Int = indexedValues.indexOf(value)
  /**
   *
   * @param idx an offset into the consistent ordering for this domain.
   * @return the Object at the specified offset in this domains consistent ordered set of values.
   * @throws IllegalArgumentException if the index is not valid for the domain.
   */
  def value(idx: Int): A = indexedValues(idx)
}

object FiniteDomain {
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 486.
   *
   * A Boolean random variable has the domain {true,false}.
   */
  val booleanDomain: FiniteDomain[Boolean] = FiniteDomain(Set(true, false))
}

/**
 * A Domain over an infinite set of discrete objects. (I.e. not real numbers)
 */
case class InfiniteDomain[A](values: Stream[A]) extends DiscreteDomain[A]