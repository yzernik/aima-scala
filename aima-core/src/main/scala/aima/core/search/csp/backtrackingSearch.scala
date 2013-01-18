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
