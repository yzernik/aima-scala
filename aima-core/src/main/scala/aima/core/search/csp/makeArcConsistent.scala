package aima.core.search.csp

import annotation.tailrec

object makeArcConsistent {
  @tailrec
  def apply(queue: List[BinaryConstraint[_, _]], csp: CSP): Option[CSP] = {
    def revise[A, B](xi: Variable[A], xj: Variable[B], rel: (A, B) => Boolean): Option[Set[A]] =
      xi.domain filter { x => xj.domain exists { rel(x, _) } } match {
        case newDomain if newDomain.size != xi.domain.size => Some(newDomain)
        case _ => None
      }

    queue match {
      case BinaryConstraint((xi, xj), rel) :: tail =>
        revise(xi, xj, rel) match {
          case Some(revisedDomain) if revisedDomain.isEmpty => None
          case Some(revisedDomain) =>
            val newCSP = csp.copy(variables = xi.copy(domain = revisedDomain) :: csp.variables filterNot { _ == xi })
            val newQueue = tail ++ (csp.neighbors(xi) map { _.variableOnRight(xi) } filterNot { _.scope._1 == xj })
            makeArcConsistent(newQueue, newCSP)
          case None => makeArcConsistent(tail, csp)
        }
      case Nil => Some(csp)
    }
  }
}
