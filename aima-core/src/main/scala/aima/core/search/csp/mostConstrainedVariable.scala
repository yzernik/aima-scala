package aima.core.search.csp

object mostConstrainedVariable {
  def apply(csp: CSP): Variable[_] = csp.variables.maxBy { variable => csp.constraints count { _.contains(variable) } }
}