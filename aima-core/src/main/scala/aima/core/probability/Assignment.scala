package aima.core.probability

abstract class Assignment[A](val variable: RandomVariable[A], val values: Stream[A]) {
  def ⊆(other: Assignment[_]): Boolean = this match {
    case FiniteAssignment(_, myValues) => other match {
      case FiniteAssignment(_, otherValues: Set[A]) if myValues subsetOf otherValues => true
      case InfiniteAssignment(_, otherValues: Stream[A]) if myValues forall otherValues.contains => true
      case _ => false
    }
    case InfiniteAssignment(_, _) => other match {
      case InfiniteAssignment(_, possibleValues) if values eq possibleValues => true
      case _ => false
    }
    case _ => false
  }
}
case class FiniteAssignment[A](override val variable: RandomVariable[A], finiteValues: Set[A]) extends Assignment[A](variable, finiteValues.to[Stream])
case class InfiniteAssignment[A](override val variable: RandomVariable[A], override val values: Stream[A]) extends Assignment[A](variable, values)