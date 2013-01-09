package aima.example.probability.bayes

import aima.core.probability.impl.FiniteRandomVariable
import aima.core.probability.impl.FiniteDomain.booleanDomain

object RandomVariables {
  val toothacheRV: FiniteRandomVariable[Boolean] = FiniteRandomVariable("Toothache", booleanDomain)
  val cavityRV: FiniteRandomVariable[Boolean] = FiniteRandomVariable("Cavity", booleanDomain)
  val catchRV: FiniteRandomVariable[Boolean] = FiniteRandomVariable("Catch", booleanDomain)
}

