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
import aima.core.logic.fol.Defaults._
import aima.core.logic.fol.grammar._

// Need to rework to ensure renaming isnt the same as before
object standardizeSentence {
  def apply[A <: Sentence](sentence: A): A = {
    def standard(sentence: Sentence): Sentence = sentence match {
      case x: AtomicSentence => x
      case ¬(x) => ¬(standardizeSentence(x))
      case left ∨ right => standardizeQV(Set(left, right)) reduce
        {(sLeft, sRight) => standardizeSentence(left) ∨ standardizeSentence(right)}
      case left ∧ right => standardizeQV(Set(left, right)) reduce
        {(sLeft, sRight) => standardizeSentence(left) ∧ standardizeSentence(right)}
      case premise ⇾ conclusion =>
        val standardized = standardizeSentence(¬(premise) ∨ conclusion)
        ¬(standardized.left) ⇾ standardized.right
      case left ⇔ right => standardizeQV(Set(left, right)) reduce
        {(sLeft, sRight) => standardizeSentence(left) ⇔ standardizeSentence(right)}
      case ∀(variable, s) => ∀(variable, standardizeSentence(s))
      case ∃(variable, s) => ∃(variable, standardizeSentence(s))

    }
    sentence match {
      case CNFSentence(clauses) => CNFSentence(clauses map {standardizeClauseVariables(_)}).asInstanceOf[A]
      case x: Clause => standardizeClauseVariables(x).asInstanceOf[A]
      case x: Literal => standardizeClauseVariables(GeneralClause(Set(x))).literals.head.asInstanceOf[A]
      case _ => standard(sentence).asInstanceOf[A]
    }
  }
}

object standardizeClauseVariables {
  def apply[A <: Clause](clause: A)(implicit standardVariable: StandardVariable): A = {
    val θ: Map[Variable, Variable] =
      (collectClauseVariables(clause) map {v => v -> Variable(standardVariable(v))})(collection.breakOut)
    substitute(θ, clause)
  }
}

object standardizeQV {
  // Standardizes variables among quantifiers in set
  def apply(sentences: Set[Sentence])(implicit standardVariable: StandardVariable): Set[Sentence] = {
    val groupedSentences = sentences groupBy {
      case x: Quantifier => true
      case _ => false
    }

    val sameVarSentences = (groupedSentences.getOrElse(true, Set()) foldLeft Map[Variable, Set[Quantifier]]()) {
      case (sameVar, α: Quantifier) => sameVar + (α.variable → (sameVar.getOrElse(α.variable, Set()) + α))
    }

    val standardizedSentences = sameVarSentences flatMap {case (_, quantifiers) =>
      quantifiers.map[Quantifier, Set[Quantifier]] {
        case ∀(variable, s) =>
          val newVariable = Variable(standardVariable(variable))
          ∀(newVariable, substitute(Map(variable → newVariable), s))
        case ∃(variable, s) =>
          val newVariable = Variable(standardVariable(variable))
          ∃(newVariable, substitute(Map(variable → newVariable), s))
      }
    }

    groupedSentences.getOrElse(false, Set()) ++ standardizedSentences
  }
}
