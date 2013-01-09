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
