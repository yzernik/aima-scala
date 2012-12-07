package aima.core.search.csp

import util.Random.shuffle
import annotation.tailrec

object minConflictsSearch {
  def apply(csp: CSP, maxSteps: Int): Option[CSP] = {
    def minConflicting[A](assign: Assignment[A], csp: CSP): CSP = {
      val constraints = csp.constraints filter {_ contains assign.variable} flatMap {_ onLeft assign.variable}
      val value = assign.values minBy {value => (constraints map {_.countRuledOut(value, csp)}).sum}
      csp.reduceDomain(assign, value)
    }

    @tailrec
    def recur(current: CSP, count: Int): Option[CSP] = {
      if (current.allConstraintsSatisfied()) Some(current)
      else if (count == maxSteps) None
      else {
        val randomAssign = shuffle(current.assignments filterNot {current constraintsSatisfied _}).head
        val newCSP = minConflicting(randomAssign, csp)
        recur(newCSP, count + 1)
      }
    }
    recur(csp, 0)
  }
}
