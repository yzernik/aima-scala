package aima.core.search.csp

object backtrackingSearch {
  type SelectUnassignedVariable = CSP => Variable[_]
  trait OrderDomainValues {
    def apply[A](variable: Variable[A], csp: CSP): List[A]
  }
  type Inference = (Variable[_], CSP) => Option[CSP]

  def apply(select: SelectUnassignedVariable, order: OrderDomainValues, infer: Inference)(csp: CSP): Option[CSP] = {
    def backtrack[A](variable: Variable[A], csp: CSP): Option[CSP] = {
      def recur(variable: Variable[A], domain: List[A]): Option[CSP] = domain match {
        case value :: tail =>
          infer(variable, csp.reduceDomain(variable, value)) match {
            case Some(inferredCSP) =>
              backtrack(select(inferredCSP), inferredCSP) match {
                case result@Some(_) => result
                case _ => recur(variable, domain)
              }
            case None => recur(variable, domain)
          }
        case Nil => None
      }

      if (csp.constraintsSatisfied())
        Some(csp)
      else
        recur(variable, order(variable, csp))
    }
    backtrack(select(csp), csp)
  }
}
