package aima.core.probability

sealed trait Proposition {
  def scope: Set[RandomVariable[_]]
  def holdsIn(possibleWorld: World): Boolean
}

sealed abstract class AtomicProposition(val scope: Set[RandomVariable[_]]) extends Proposition
case class AssignmentProposition(assignment: Assignment[_]) extends AtomicProposition(Set(assignment.variable)) {
  def holdsIn(possibleWorld: World): Boolean = assignment match {
    case FiniteAssignment(variable, values) => possibleWorld get variable.num match {
      case Some(FiniteAssignment(_, possibleValues)) if values == possibleValues => true
      case _ => false
    }
    case InfiniteAssignment(variable, values) => possibleWorld get variable.num match {
      case Some(InfiniteAssignment(_, possibleValues)) if values eq possibleValues => true
      case _ => false
    }
  }
}
case class SubsetProposition(assignment: Assignment[_]) extends AtomicProposition(Set(assignment.variable)) {
  def holdsIn(possibleWorld: World): Boolean = possibleWorld get assignment.variable.num match {
    case Some(assign) => assign ⊆ assignment
    case None => false
  }
}
case class ¬(proposition: Proposition) extends AtomicProposition(proposition.scope) {
  def holdsIn(possibleWorld: World): Boolean = !(proposition holdsIn possibleWorld)
}
case class EquivalentProposition(variables: Set[RandomVariable[_]]) extends AtomicProposition(variables) {
  require(variables.size >= 2)
  def holdsIn(possibleWorld: World): Boolean = (variables groupBy {possibleWorld get _.num}).keySet.size == 1
}

sealed abstract class ComplexProposition(propositions: Set[Proposition]) extends Proposition {
  val scope = propositions flatMap {_.scope}
}
sealed abstract class BinarySentenceProposition(left: Proposition, right: Proposition)
  extends ComplexProposition(Set(left, right)) {
  def holdsIn(possibleWorld: World): Boolean = operator(left holdsIn possibleWorld, right holdsIn possibleWorld)
  def operator(left: Boolean, right: Boolean): Boolean
}
object Connective {
  case class ∧(left: Proposition, right: Proposition) extends BinarySentenceProposition(left, right) {
    def operator(left: Boolean, right: Boolean) = left && right
  }
  case class ∨(left: Proposition, right: Proposition) extends BinarySentenceProposition(left, right) {
    def operator(left: Boolean, right: Boolean) = left || right
  }
}

object Proposition {
  implicit final class ProbabilityProposition(left: Proposition) {
    def ∧(right: Proposition): Connective.∧ = Connective.∧(left, right)
    def ∨(right: Proposition): Connective.∨ = Connective.∨(left, right)
  }
}

