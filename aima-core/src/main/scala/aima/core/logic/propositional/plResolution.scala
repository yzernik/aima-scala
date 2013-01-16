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

import aima.core.logic.propositional.grammar.PropositionalLogic
import scala.annotation.tailrec

object plResolution {
  def apply(sentenceToCNF: SentenceToCNF, plResolve: PLResolve)(kb: KnowledgeBase[_], α: Sentence): Boolean = {
    @tailrec
    def recur(clauses: Set[Clause]): Boolean = {
      @tailrec
      def recur2(pairs: List[(Clause, Clause)], newSet: Set[Clause]): Option[Set[Clause]] = pairs match {
        case (left, right) :: tail =>
          val resolvents = plResolve(left, right)
          if (resolvents.exists(_.literals.isEmpty)) None else recur2(tail, newSet ++ resolvents)
        case Nil => Some(newSet)
      }
      val pairs = clauses.to[Seq] combinations 2 map {case Seq(left, right) => (left, right)}
      recur2(pairs.to[List], Set()) match {
        case Some(s) if s.subsetOf(clauses) => false
        case Some(s) => recur(clauses ++ s)
        case None => true
      }
    }
    recur(sentenceToCNF(kb.asSentence ∧ ¬(α)).clauses)
  }
}
