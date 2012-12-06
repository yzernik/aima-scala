package aima.core.search.csp

import aima.core.search.csp.backtrackingSearch.OrderDomainValues

object leastConstrainingValue extends OrderDomainValues {
  def apply[A](variable: Variable[A], csp: CSP): List[A] = {
    val constraints = csp.constraints filter { _.contains(variable) } map { constraint => constraint.onLeft(variable) }
    variable.domain.to[List] sortBy { value => (constraints map { _.countRuledOut(value, csp) }).sum }
  }
}
