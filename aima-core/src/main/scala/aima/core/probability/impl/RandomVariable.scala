package aima.core.probability.impl

import aima.core.probability.RandomVariable

/**
 * A RandomVariable with a FiniteDomain.
 *
 * @param num unique number associated with this variable
 * @param domain that this variable can take on
 * @tparam A type of the domain
 * @author Alex DiCarlo
 */
case class FiniteRandomVariable[A](num: Int, domain: FiniteDomain[A]) extends RandomVariable[A] {
  lazy val finiteValues: Set[A]  = domain.finiteValues
  lazy val indexedValues: IndexedSeq[A] = domain.indexedValues
}

/**
 * A RandomVariable with an InfiniteDomain
 *
 * @param num unique number associated with this variable
 * @param domain that this variable can take on
 * @tparam A type of the domain
 * @author Alex DiCarlo
 */
case class InfiniteRandomVariable[A](num: Int, domain: InfiniteDomain[A]) extends RandomVariable[A]
