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

import aima.core.logic.fol.Connective._
import aima.core.logic.fol.grammar.FOLLogic

object sentenceToCNF {
  def apply(sentence: Sentence)(implicit standardFunction: StandardFunction): CNFSentence = {
    def removeImplications(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => ¬(removeImplications(x))
      case left ∨ right => removeImplications(left) ∨ removeImplications(right)
      case left ∧ right => removeImplications(left) ∧ removeImplications(right)
      case premise ⇾ conclusion => ¬(removeImplications(premise)) ∨ removeImplications(conclusion)
      case left ⇔ right => removeImplications((left ⇾ right) ∧ (right ⇾ left))
      case ∀(symbol, s) => ∀(symbol, removeImplications(s))
      case ∃(symbol, s) => ∃(symbol, removeImplications(s))
    }

    def negationsIn(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => negationsIn(grammar.negate(x))
      case left ∨ right => negationsIn(left) ∨ negationsIn(right)
      case left ∧ right => negationsIn(left) ∧ negationsIn(right)
      case ∀(symbol, s) => ∀(symbol, negationsIn(s))
      case ∃(symbol, s) => ∃(symbol, negationsIn(s))
    }

    def removeExistentials(sentence: Sentence): Sentence = {
      def collectSentenceFreeVariables(bound: Set[Variable], sentence: Sentence): Set[Variable] = sentence match {
        case x: AtomicSentence => x.terms flatMap {
          case a: Variable if bound(a) => Some(a)
          case _ => None
        }
        case ¬(x) => collectSentenceFreeVariables(bound, x)
        case left ∨ right => collectSentenceFreeVariables(bound, left) ++ collectSentenceFreeVariables(bound, right)
        case left ∧ right => collectSentenceFreeVariables(bound, left) ++ collectSentenceFreeVariables(bound, right)
        case x: Quantifier => collectSentenceFreeVariables(bound + x.variable, x.sentence)
      }
      sentence match {
        case x: AtomicSentence => x
        case ¬(x) => ¬(removeExistentials(x))
        case left ∨ right => removeExistentials(left) ∨ removeExistentials(right)
        case left ∧ right => removeExistentials(left) ∧ removeExistentials(right)
        case ∀(variable, s) => ∀(variable, removeExistentials(s))
        case ∃(variable, s) =>
          val freeVariables: Set[Term] = collectSentenceFreeVariables(Set(), s).toSet
          substitute(Map(variable → standardFunction(freeVariables)), sentence)
      }
    }

    def removeUniversals(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => ¬(removeUniversals(x))
      case left ∨ right => removeUniversals(left) ∨ removeUniversals(right)
      case left ∧ right => removeUniversals(left) ∧ removeUniversals(right)
      case ∀(variable, s) => removeUniversals(s)
    }

    def distrib(sentence: Sentence): Set[Clause] = sentence match {
      case x: AtomicSentence => Set(GeneralClause(Set(PositiveLiteral(x))))
      case ¬(x: AtomicSentence) => Set(GeneralClause(Set(NegativeLiteral(x))))
      case left ∧ right => distrib(left) ++ distrib(right)
      case left ∨ right =>
        val leftClauses = distrib(left)
        val rightClauses = distrib(right)
        leftClauses flatMap
          {lclause => rightClauses map {rclause => GeneralClause(lclause.literals ++ rclause.literals)}}
    }

    val eliminateImplications = removeImplications(sentence)
    val moveNotInwards = negationsIn(eliminateImplications)
    val standardizeVariables = standardizeSentence(moveNotInwards)
    val skolemized = removeExistentials(standardizeVariables)
    val droppedUniversals = removeUniversals(skolemized)
    val distributed = distrib(droppedUniversals)
    CNFSentence(distributed)
  }
}
