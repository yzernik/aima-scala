package aima.core.search.csp

object ac3 {
  def apply(csp: CSP): Option[CSP] = makeArcConsistent(csp.constraints ++ csp.constraints map { _.reverse }, csp)
}
