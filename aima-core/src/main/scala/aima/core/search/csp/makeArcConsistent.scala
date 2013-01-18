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
