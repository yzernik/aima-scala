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

import aima.core.probability.RandomVariable

/**
 * A RandomVariable with a FiniteDomain.
 *
 * @param name unique name associated with this variable
 * @param domain that this variable can take on
 * @tparam A type of the domain
 * @author Alex DiCarlo
 */
case class FiniteRandomVariable[A](name: String, domain: FiniteDomain[A]) extends RandomVariable[A] {
  lazy val finiteValues: Set[A]  = domain.finiteValues
  lazy val indexedValues: IndexedSeq[A] = domain.indexedValues
}

/**
 * A RandomVariable with an InfiniteDomain
 *
 * @param name unique name associated with this variable
 * @param domain that this variable can take on
 * @tparam A type of the domain
 * @author Alex DiCarlo
 */
case class InfiniteRandomVariable[A](name: String, domain: InfiniteDomain[A]) extends RandomVariable[A]
