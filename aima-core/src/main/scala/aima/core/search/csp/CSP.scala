package aima.core.search.csp

case class CSP(variables: List[Variable[_]], constraints: List[BinaryConstraint[_, _]]) {

  def neighbors(variable: Variable[_]): List[BinaryConstraint[_, _]] = constraints filter { _.contains(variable) }

  def constraintsSatisfied(): Boolean = variables forall { _.domain.size == 1 }

  def reduceDomain[A](variable: Variable[A], value: A): CSP =
    this.copy(variables = (variables filterNot { _ == variable }) ++ List(Variable(variable.num, Set(value))))
}
