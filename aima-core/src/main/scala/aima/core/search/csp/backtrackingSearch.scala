package aima.core.search.csp

import annotation.tailrec

object backtrackingSearch {
  type SelectUnassignedVariable = CSP => Assignment[_]
  trait OrderValues {
    def apply[A](assignment: Assignment[A], csp: CSP): List[A]
  }
  type Inference = (Assignment[_], CSP) => Option[CSP]

  def apply(select: SelectUnassignedVariable, order: OrderValues, infer: Inference)(csp: CSP): Option[CSP] = {
    def backtrack[A](assignment: Assignment[A], csp: CSP): Option[CSP] = {
      @tailrec
      def recur(assignment: Assignment[A], domain: List[A]): Option[CSP] = domain match {
        case value :: tail =>
          infer(assignment, csp.reduceDomain(assignment, value)) match {
            case Some(inferredCSP) =>
              backtrack(select(inferredCSP), inferredCSP) match {
                case result@Some(_) => result
                case _ => recur(assignment, domain)
              }
            case None => recur(assignment, domain)
          }
        case Nil => None
      }

      if (csp.allConstraintsSatisfied()) Some(csp) else recur(assignment, order(assignment, csp))
    }
    backtrack(select(csp), csp)
  }
}
