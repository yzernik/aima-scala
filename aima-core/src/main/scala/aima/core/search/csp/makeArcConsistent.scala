package aima.core.search.csp

import annotation.tailrec

object makeArcConsistent {
  @tailrec
  def apply(queue: List[BinaryConstraint[_, _]], csp: CSP): Option[CSP] = {
    def revise[A, B](xi: Assignment[A], xj: Assignment[B], rel: (A, B) => Boolean): Option[Set[A]] = {
      xi.values filter {x => xj.values exists {rel(x, _)}} match {
        case newDomain if newDomain.size != xi.values.size => Some(newDomain)
        case _ => None
      }
    }
    queue match {
      case BinaryConstraint((xi, xj), rel) :: tail =>
        val xiAssign = csp.getAssignment(xi).get
        val xjAssign = csp.getAssignment(xj).get
        revise(xiAssign, xjAssign, rel) match {
          case Some(revisedValues) if revisedValues.isEmpty => None
          case Some(revisedValues) =>
            val newXiAssign = xiAssign.copy(values = revisedValues)
            val newCSP = csp.copy(assignments = newXiAssign :: csp.assignments filterNot {_ == xiAssign})
            val newQueue = tail ++ (csp.neighbors(xi) flatMap {_ onRight xi} filterNot {_.scope._1 == xj})
            makeArcConsistent(newQueue, newCSP)
          case None => makeArcConsistent(tail, csp)
        }
      case Nil => Some(csp)
    }
  }
}
