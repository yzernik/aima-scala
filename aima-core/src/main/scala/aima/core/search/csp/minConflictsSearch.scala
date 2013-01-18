/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

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
