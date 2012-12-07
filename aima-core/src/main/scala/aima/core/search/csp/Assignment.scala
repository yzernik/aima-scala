package aima.core.search.csp

case class Assignment[A](variable: Variable[A], values: Set[A])