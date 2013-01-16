/*
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

package aima.core.logic.propositional

import scala.annotation.tailrec

object plResolve extends PLResolve {
  def apply(c1: Clause, c2: Clause): Set[Clause] = {
    @tailrec
    def recur(c1Literals: List[Literal], result: Set[Clause]): Set[Clause] = c1Literals match {
      case (literal: PositiveLiteral) :: tail if c2.literals exists {_ == NegativeLiteral(literal.symbol)} =>
        recur(tail, result + Clause((c1.literals - literal) ++ (c2.literals - NegativeLiteral(literal.symbol))))
      case (literal: PositiveLiteral) :: tail =>
        recur(tail, result)
      case (literal: NegativeLiteral) :: tail if c2.literals exists {_ == PositiveLiteral(literal.symbol)} =>
        recur(tail, result + Clause((c1.literals - literal) ++ (c2.literals - PositiveLiteral(literal.symbol))))
      case (literal: NegativeLiteral) :: tail =>
        recur(tail, result)
      case Nil =>
        result
    }
    val resolvents = recur(c1.literals.to[List], Set())

    resolvents filterNot {
      _.literals.to[Seq] combinations 2 exists {
        case Seq(PositiveLiteral(x), NegativeLiteral(y)) if x == y => true
        case Seq(NegativeLiteral(x), PositiveLiteral(y)) if x == y => true
        case _ => false
      }
    }
  }
}
