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

package aima.core.logic.propositional

import aima.core.logic.propositional.Connective._
import aima.core.logic.propositional.grammar.PropositionalLogic

object sentenceToCNF extends SentenceToCNF {
  implicit def apply(sentence: Sentence): CNFSentence = {
    def removeImplications(sentence: Sentence): Sentence = sentence match {
      case x: PropositionSymbol => x
      case ¬(x) => ¬(removeImplications(x))
      case left ∨ right => removeImplications(left) ∨ removeImplications(right)
      case left ∧ right => removeImplications(left) ∧ removeImplications(right)
      case premise ⇾ conclusion => ¬(removeImplications(premise)) ∨ removeImplications(conclusion)
      case left ⇔ right => removeImplications((left ⇾ right) ∧ (right ⇾ left))
    }

    def negationsIn(sentence: Sentence): Sentence = sentence match {
      case x: PropositionSymbol => x
      case ¬(x) => negationsIn(negate(x))
      case left ∨ right => negationsIn(left) ∨ negationsIn(right)
      case left ∧ right => negationsIn(left) ∧ negationsIn(right)
    }

    def negate(sentence: Sentence): Sentence = sentence match {
      case x: PropositionSymbol => ¬(x)
      case ¬(x) => x
      case left ∨ right => ¬(left) ∧ ¬(right)
      case left ∧ right => ¬(left) ∨ ¬(right)
    }

    def distrib(sentence: Sentence): Set[Clause] = sentence match {
      case x: PropositionSymbol => Set(Clause(Set(PositiveLiteral(x))))
      case ¬(x: PropositionSymbol) => Set(Clause(Set(NegativeLiteral(x))))
      case left ∧ right => distrib(left) ++ distrib(right)
      case left ∨ right =>
        val leftClauses = distrib(left)
        val rightClauses = distrib(right)
        leftClauses flatMap {lclause => rightClauses map {rclause => Clause(lclause.literals ++ rclause.literals)}}
    }

    CNFSentence(distrib(negationsIn(removeImplications(sentence))))
  }
}
