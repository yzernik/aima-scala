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

package aima.core.logic.fol

import scala.annotation.tailrec

object folFCAsk {
  def apply(kb: KnowledgeBase, α: Sentence)(implicit standardVariable: StandardVariable): Option[Substitution] = {
    def fetch(s: AtomicSentence, set: Set[AtomicSentence]): Set[Map[Variable, Term]] = set flatMap {unify(s, _)}
    def invert[T <: Literal](literals: Set[T]): Set[Literal] = literals map {
      case PositiveLiteral(s) => NegativeLiteral(s)
      case NegativeLiteral(s) => PositiveLiteral(s)
    }

    @tailrec
    def recur(clauses: List[ImplicationDefinite], newSet: Set[AtomicSentence]): Option[Substitution] = clauses match {
      case clause :: tail =>
        val standardized = standardizeSentence(clause)
        @tailrec
        def recur2(
          thetas: List[Substitution],
          newSet: Set[AtomicSentence]): (Set[AtomicSentence], Option[Substitution]) = thetas match {
          case θ :: tail2 =>
            val qPrime = substitute(θ, standardized.conclusion)
            if (kb.fetch(qPrime).isEmpty && fetch(qPrime, newSet).isEmpty) {
              unify(qPrime, α) match {
                case Some(substitution) => (newSet, Some(substitution))
                case None => recur2(tail2, newSet + qPrime.sentence)
              }
            } else {
              recur2(tail2, newSet)
            }
          case Nil => (newSet, None)
        }
        recur2(kb.fetch(invert(standardized.premises).toList).toList, newSet) match {
          case (resultNewSet, Some(substitution)) => Some(substitution)
          case (resultNewSet, None) => recur(tail, resultNewSet)
        }
      case Nil if newSet.isEmpty => None
      case Nil =>
        kb.tell(newSet)
        recur(kb.implicationDefinites.toList, Set())
    }
    recur(kb.implicationDefinites.toList, Set())
  }
}
