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

object folBCAsk {
  def apply(kb: KnowledgeBase, α: AtomicSentence): Set[Substitution] = {
    folBCOr(kb, α, Map())
  }

  def folBCOr(kb: KnowledgeBase, goal: AtomicSentence, θ: Substitution): Set[Substitution] = {
    @tailrec
    def recur(rules: List[ImplicationDefinite], partial: Set[Substitution]): Set[Substitution] = rules match {
      case rule :: tail =>
        val standardized = standardizeSentence(rule)
        val (lhs, rhs) = (standardized.premises.toList map {_.sentence}, standardized.conclusion.sentence)
        recur(tail, partial ++ folBCAnd(kb, lhs, unify(rhs, goal, θ)))
      case Nil => partial
    }
    val rulesForGoal = kb.implicationDefinites filter {clause => unify(clause.conclusion.sentence, goal).isDefined}
    recur(rulesForGoal.toList, Set())
  }

  def folBCAnd(kb: KnowledgeBase, goals: List[AtomicSentence], θ: Option[Substitution]): Set[Substitution] =
    (θ, goals) match {
      case (None, _) => Set()
      case (Some(substitution), Nil) => Set(substitution)
      case (Some(substitution), first :: rest) =>
        folBCOr(kb, substitute(substitution, first), substitution) flatMap {sub => folBCAnd(kb, rest, Some(sub))}
    }
}
