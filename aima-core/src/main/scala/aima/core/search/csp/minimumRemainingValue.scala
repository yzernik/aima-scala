package aima.core.search.csp

object minimumRemainingValue {
  def apply(csp: CSP): Variable[_] = csp.variables minBy { _.domain.size }
}
