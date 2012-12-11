package aima.core.search.csp

import annotation.tailrec

object treeCSPSearch {
  def apply(topologicalSort: TopologicalSort)(csp: CSP): Option[CSP] = {
    @tailrec
    def recur(topologicalOrdering: List[TreeAssignment[_]], csp: CSP): Option[CSP] = topologicalOrdering match {
      case TreeAssignment(parent, variable, values) :: tail =>
        val constraints = csp.constraints filter {
          constraint => constraint.contains(parent.variable) && constraint.contains(variable)
        }
        makeArcConsistent(constraints, csp) match {
          case Some(revisedCSP) => recur(tail, revisedCSP)
          case None => None
        }
      case Nil => Some(csp)
    }
    recur(topologicalSort(csp.assignments, csp.assignments.head), csp)
  }

  type TopologicalSort = (Traversable[Assignment[_]], Assignment[_]) => List[TreeAssignment[_]]
  case class TreeAssignment[A](parent: Assignment[_], variable: Variable[A], values: Set[A])
}
