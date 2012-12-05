package aima.core.search.csp

/**
 * @param scope pair of variables participating in this constraint
 * @param rel function that returns true on a valid assignment
 * @tparam A type of domain for first variable
 * @tparam B type of domain for second variable
 * @tparam V type of variable
 *
 *           Author: Alex DiCarlo (dicarlo2)
 *           Date: 11/23/12
 */
case class BinaryConstraint[A, B, V[_]](scope: (V[A], V[B]), rel: BinaryRelation[A, B, V]) {
  lazy val reverse: BinaryConstraint[B, A, V] =
    BinaryConstraint[B, A, V](scope.swap, (assignB, assignA) => rel(assignA, assignB))

  def contains(variable: V[_]): Boolean = scope._1 == variable || scope._2 == variable

  def neighborToA(variable: V[A]): Option[V[B]] = scope match {
    case (v, neighbor) if neighbor != variable && v == variable => Some(neighbor)
    case _ => None
  }

  def neighborToB(variable: V[B]): Option[V[A]] = scope match {
    case (neighbor, v) if neighbor != variable && v == variable => Some(neighbor)
    case _ => None
  }

  def constraintA[T](variable: V[T]): BinaryConstraint[T, _, V] = this.asInstanceOf[BinaryConstraint[T, _, V]]
}

object BinaryConstraint {
  def filterConstraints[A, V[_]](
    variable: V[A],
    constraints: Set[BinaryConstraint[_, _, V]]): Set[BinaryConstraint[A, _, V]] =
    constraints filterNot { constraint => constraint.contains(variable) } map {
      case constraint@BinaryConstraint((v, _), _) if v == variable =>
        constraint.asInstanceOf[BinaryConstraint[A, _, V]]
      case constraint@BinaryConstraint((_, v), _) if v == variable =>
        constraint.reverse.asInstanceOf[BinaryConstraint[A, _, V]]
    }

  def testFilter[A, B, V[_]](variable: V[A]) {
    val constraints = Set[BinaryConstraint[_, _, V]]
  }
}