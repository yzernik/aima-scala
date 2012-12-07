package aima.core.search.csp

object minimumRemainingValue {
  def apply(csp: CSP): Assignment[_] = csp.assignments minBy { _.values.size }
}
