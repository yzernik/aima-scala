package aima.core.probability.impl

import aima.core.probability._

case class FiniteSingleWorld(assignments: Map[FiniteRandomVariable[_], SingleAssignment[_]]) extends World {
  type RV[A] = FiniteRandomVariable[A]
  type Assign[A] = SingleAssignment[A]
}
