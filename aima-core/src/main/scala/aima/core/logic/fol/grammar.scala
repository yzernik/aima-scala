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

package aima.core.logic.fol

import aima.core.logic.fol.Connective.{∧, ∨}

abstract class Sentence(val terms: Set[Term])

abstract class AtomicSentence(terms: Set[Term]) extends Sentence(terms)
case class Predicate(symbol: String, override val terms: Set[Term]) extends AtomicSentence(terms)
object Predicate {
  val True = Predicate("True", Set())
  val False = Predicate("False", Set())
}
case class TermEqual(left: Term, right: Term) extends AtomicSentence(Set(left, right))

abstract class ComplexSentence(val sentences: Set[Sentence]) extends Sentence(sentences flatMap {_.terms}) {
  def args: List[Sentence] = sentences.to[List]
}
case class ¬(sentence: Sentence) extends ComplexSentence(Set(sentence))

sealed abstract class Connective(left: Sentence, right: Sentence) extends ComplexSentence(Set(left, right))
object Connective {
  case class ∧(left: Sentence, right: Sentence) extends Connective(left, right)
  case class ∨(left: Sentence, right: Sentence) extends Connective(left, right)
  case class ⇾(premise: Sentence, conclusion: Sentence) extends Connective(premise, conclusion)
  case class ⇔(left: Sentence, right: Sentence) extends Connective(left, right)
}
sealed abstract class Quantifier(val variable: Variable, val sentence: Sentence) extends ComplexSentence(Set(sentence))
case class ∀(override val variable: Variable, override val sentence: Sentence) extends Quantifier(variable, sentence)
case class ∃(override val variable: Variable, override val sentence: Sentence) extends Quantifier(variable, sentence)

sealed abstract class Term(val symbol: String)
sealed abstract class AFunction(symbol: String, val terms: Set[Term]) extends Term(symbol)
case class Function(override val symbol: String, override val terms: Set[Term]) extends AFunction(symbol, terms)
case class ConstantFunction(override val symbol: String) extends AFunction(symbol, Set())
case class Variable(override val symbol: String) extends Term(symbol)

object grammar {
  implicit final class FOLLogic(left: Sentence) {
    def ∧(right: Sentence): Connective.∧ = Connective.∧(left, right)
    def ∨(right: Sentence): Connective.∨ = Connective.∨(left, right)
    def ⇾(right: Sentence): Connective.⇾ = Connective.⇾(left, right)
    def ⇔(right: Sentence): Connective.⇔ = Connective.⇔(left, right)
  }

  def negate(sentence: Sentence): Sentence = sentence match {
    case x: AtomicSentence => ¬(x)
    case ¬(x) => x
    case left ∨ right => ¬(left) ∧ ¬(right)
    case left ∧ right => ¬(left) ∨ ¬(right)
    case ∀(symbol, s) => ∃(symbol, ¬(s))
    case ∃(symbol, s) => ∀(symbol, ¬(s))
  }
}