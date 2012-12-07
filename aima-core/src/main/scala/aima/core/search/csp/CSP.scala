package aima.core.search.csp

case class CSP(assignments: List[Assignment[_]], constraints: List[BinaryConstraint[_, _]]) {
  def neighbors(variable: Variable[_]): List[BinaryConstraint[_, _]] = constraints filter {_ contains variable}

  def constraintsSatisfied[A](assign: Assignment[A]): Boolean = {
    neighbors(assign.variable) forall {
      constraint =>
        assign.values forall {
          value =>
            (constraint onLeft assign.variable map {_.countRuledOut(value, this)} getOrElse 0) == 0
        }
    }
  }

  def allConstraintsSatisfied(): Boolean = assignments forall {constraintsSatisfied(_)}

  def conflictingAssignments(): List[Assignment[_]] = assignments filterNot {constraintsSatisfied(_)}

  def getAssignment[A](variable: Variable[A]): Option[Assignment[A]] = {
    assignments.flatMap[Assignment[A], List[Assignment[A]]] {
      case assign: Assignment[A] if variable == assign.variable => Some(assign)
      case _ => None
    }.headOption
  }

  def getCurrentDomain[A](variable: Variable[A]): Set[A] = getAssignment(variable) map {_.values} getOrElse Set()

  def reduceDomain[A](assign: Assignment[A], value: A): CSP =
    this.copy(assignments = assign.copy(values = Set(value)) :: (assignments filterNot {_ == assign}))
}
