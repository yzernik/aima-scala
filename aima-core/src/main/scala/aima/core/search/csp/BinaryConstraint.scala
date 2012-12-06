package aima.core.search.csp

case class BinaryConstraint[A, B](scope: (Variable[A], Variable[B]), rel: (A, B) => Boolean) {
  // todo: why can't it infer this without explicitly putting type arguments
  lazy val reverse = BinaryConstraint[B, A](scope.swap, (b, a) => rel(a, b))

  def contains(variable: Variable[_]): Boolean = scope._1 == variable || scope._2 == variable

  def countRuledOut(leftValue: A, csp: CSP): Int = {
    val rightDomain: Set[B] = csp.variables.find(_ == scope._2).get.asInstanceOf[Variable[B]].domain
    rightDomain.count { rightValue => !rel(leftValue, rightValue) }
  }

  def onRight[C](variable: Variable[C]): BinaryConstraint[_, C] = {
    if (scope._1 == variable)
      reverse.asInstanceOf[BinaryConstraint[_, C]]
    else
      this.asInstanceOf[BinaryConstraint[_, C]]
  }

  def onLeft[C](variable: Variable[C]): BinaryConstraint[C, _] = {
    if (scope._1 == variable)
      this.asInstanceOf[BinaryConstraint[C, _]]
    else
      reverse.asInstanceOf[BinaryConstraint[C, _]]
  }
}
