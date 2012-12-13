package aima.core.probability

sealed abstract class Domain[A] {
  def values: Stream[A]
}

case class FiniteDomain[A](finiteValues: Set[A]) extends Domain[A] {
  lazy val indexedValues = finiteValues.to[IndexedSeq]
  def values: Stream[A] = finiteValues.to[Stream]
}

case class OrderedFiniteDomain[A <% Ordered[A]](finiteValues: Seq[A]) extends Domain[A] {
  lazy val indexedValues = finiteValues.to[IndexedSeq]
  def values: Stream[A] = finiteValues.to[Stream]
}

case class InfiniteDomain[A](values: Stream[A]) extends Domain[A]
case class OrderedInfiniteDomain[A <% Ordered[A]](values: Stream[A]) extends Domain[A]
