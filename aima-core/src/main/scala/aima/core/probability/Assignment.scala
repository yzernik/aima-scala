package aima.core.probability

trait Assignment[A] {
  val variable: RandomVariable[A]
  val values: Set[A]
  require(variable.values.contains(values), "RandomVariable domain does not contain all values in assignment")
  def ⊆[B](other: Assignment[B]): Boolean = values forall {value => other.values exists {_ == value}}
}