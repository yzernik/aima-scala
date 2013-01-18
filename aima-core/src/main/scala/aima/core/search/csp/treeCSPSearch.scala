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
