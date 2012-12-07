package aima.core.search.csp

import aima.core.search.csp.backtrackingSearch.OrderValues

object leastConstrainingValue extends OrderValues {
  def apply[A](assign: Assignment[A], csp: CSP): List[A] = {
    val constraints = csp.constraints filter {_ contains assign.variable} flatMap {_ onLeft assign.variable}
    assign.values.to[List] sortBy {value => (constraints map {_.countRuledOut(value, csp)}).sum}
  }
}
