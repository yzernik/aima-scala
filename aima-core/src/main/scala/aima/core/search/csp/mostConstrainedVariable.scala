package aima.core.search.csp

object mostConstrainedVariable {
  def apply(csp: CSP): Assignment[_] =
    csp.assignments maxBy {assign => csp.constraints count {_ contains assign.variable}}
}