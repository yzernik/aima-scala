package aima.core.search.csp

case class BinaryConstraint[A, B](scope: (Variable[A], Variable[B]), rel: (A, B) => Boolean) {
  lazy val reverse = BinaryConstraint[B, A](scope.swap, (b, a) => rel(a, b))

  def contains(variable: Variable[_]): Boolean = scope._1 == variable || scope._2 == variable

  def countRuledOut(leftAssignment: A, csp: CSP): Int =
    csp getCurrentDomain scope._2 count {rightAssignment => !rel(leftAssignment, rightAssignment)}

  def onRight[C](variable: Variable[C]): Option[BinaryConstraint[_, C]] = this match {
    case onRight: BinaryConstraint[_, C] if scope._2 == variable => Some(onRight)
    case onLeft: BinaryConstraint[C, _] if scope._1 == variable => reverse.onRight[C](variable)
    case _ => None
  }

  def onLeft[C](variable: Variable[C]): Option[BinaryConstraint[C, _]] = this match {
    case onRight: BinaryConstraint[_, C] if scope._2 == variable => reverse.onLeft[C](variable)
    case onLeft: BinaryConstraint[C, _] if scope._1 == variable => Some(onLeft)
    case _ => None
  }
}
