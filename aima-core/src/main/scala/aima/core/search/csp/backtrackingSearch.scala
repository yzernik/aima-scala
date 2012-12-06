package aima.core.search.csp

object backtrackingSearch {
  type SelectUnassignedVariable = CSP => Variable[_]
  type OrderDomainValues = (Variable[_], CSP) => Set[_]
  type Inference = (Variable[_], CSP) => Option[CSP]

  def apply(select: SelectUnassignedVariable, order: OrderDomainValues, infer: Inference)(csp: CSP): Option[CSP] = {
    def backtrack(csp: CSP): Option[CSP] = {
      if (csp.constraintsSatisfied()) return Some(csp)
      val variable = select(csp)
      def recur(variable: Variable[_], domain: List[_]): Option[CSP] = domain match {
        case value :: tail =>
          infer(variable, csp.reduceDomain(variable, value)) match {
            case Some(inferredCSP) =>
              backtrack(inferredCSP) match {
                case result@Some(_) => result
                case _ => recur(variable, tail)
              }
            case None =>
              recur(variable, tail)
          }
        case Nil => None
      }
      recur(variable, order(variable, csp).to[List])
    }
    backtrack(csp)
  }
}
