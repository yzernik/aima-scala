package aima.core.search.csp

case class CSP(variables: List[Variable[_]], constraints: List[BinaryConstraint[_, _]]) {
  def neighbors(variable: Variable[_]): List[BinaryConstraint[_, _]] = constraints filter { _.contains(variable) }
}
