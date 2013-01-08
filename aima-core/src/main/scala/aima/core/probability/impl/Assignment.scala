package aima.core.probability.impl

import aima.core.probability.{Assignment, RandomVariable}

object Assignment {
  def unapply[A](assignment: Assignment[A]): Option[(RandomVariable[A], Set[A])] = {
    Some(assignment.variable, assignment.values)
  }
}

case class SingleAssignment[A](variable: RandomVariable[A], value: A) extends Assignment[A] {
  val values: Set[A] = Set(value)
}
case class SetAssignment[A](variable: RandomVariable[A], values: Set[A]) extends Assignment[A]
