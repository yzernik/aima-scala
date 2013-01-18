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

import java.util.regex.Pattern

sealed abstract class Sentence {
  def isTrueIn(model: Model): Boolean
  def isFalseIn(model: Model): Boolean
  def symbols: Set[PropositionSymbol]
}

sealed abstract class PropositionSymbol(val symbol: String) extends Sentence {
  require(PropositionSymbol.compiledSymbolPattern.matcher(symbol).matches(),
    s"Symbol $symbol did not match required regex [A-Z][a-z0-9A-Z]* for symbols")
  def isTrueIn(model: Model): Boolean = model get this getOrElse false
  def isFalseIn(model: Model): Boolean = !(model get this getOrElse true)
  lazy val symbols: Set[PropositionSymbol] = Set(this)
}

private object PropositionSymbol {
  val compiledSymbolPattern: Pattern = """[A-Z][a-z0-9A-Z]*""".r.pattern
}

case class PSymbol(override val symbol: String) extends PropositionSymbol(symbol) {
  override val toString: String = symbol
}

case object True extends PropositionSymbol("True") {
  override def isTrueIn(model: Model): Boolean = true
  override def isFalseIn(model: Model): Boolean = false
  override val toString: String = "True"
}

case object False extends PropositionSymbol("False") {
  override def isTrueIn(model: Model): Boolean = false
  override def isFalseIn(model: Model): Boolean = true
  override val toString: String = "False"
}

sealed abstract class ComplexSentence(sentences: Traversable[Sentence]) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = (sentences flatMap {_.symbols}).to[Set]
}

case class ¬(sentence: Sentence) extends ComplexSentence(List(sentence)) {
  def isTrueIn(model: Model): Boolean = sentence isFalseIn model
  def isFalseIn(model: Model): Boolean = sentence isTrueIn model
  override val toString: String = s"(~$sentence)"
}

sealed abstract class Connective(left: Sentence, right: Sentence) extends ComplexSentence(List(left, right)) {
  def isTrueIn(model: Model): Boolean = operator(left isTrueIn model, right isTrueIn model)
  def isFalseIn(model: Model): Boolean = operator(left isFalseIn model, right isFalseIn model)
  def operator(left: Boolean, right: Boolean): Boolean
}

object Connective {
  case class ∧(left: Sentence, right: Sentence) extends Connective(left, right) {
    def operator(left: Boolean, right: Boolean): Boolean = left && right
    override val toString: String = s"($left & $right)"
  }

  case class ∨(left: Sentence, right: Sentence) extends Connective(left, right) {
    def operator(left: Boolean, right: Boolean): Boolean = left || right
    override val toString: String = s"($left | $right)"
  }

  case class ⇾(premise: Sentence, conclusion: Sentence) extends Connective(premise, conclusion) {
    def operator(left: Boolean, right: Boolean): Boolean = (left && right) || !left
    override val toString: String = s"($premise => $conclusion)"
  }

  case class ⇔(left: Sentence, right: Sentence) extends Connective(left, right) {
    def operator(left: Boolean, right: Boolean): Boolean = (left && right) || (!left && !right)
    override val toString: String = s"($left <=> $right)"
  }
}

object grammar {
  implicit final class PropositionalLogic(left: Sentence) {
    def ∧(right: Sentence): Connective.∧ = Connective.∧(left, right)
    def ∨(right: Sentence): Connective.∨ = Connective.∨(left, right)
    def ⇾(right: Sentence): Connective.⇾ = Connective.⇾(left, right)
    def ⇔(right: Sentence): Connective.⇔ = Connective.⇔(left, right)
  }

  implicit def booleanToPropositionalSymbol(bool: Boolean): PropositionSymbol = if (bool) True else False
}

case class CNFSentence(clauses: Set[Clause]) extends ComplexSentence(clauses) {
  def isTrueIn(model: Model): Boolean = clauses forall {_ isTrueIn model}
  def isFalseIn(model: Model): Boolean = clauses exists {_ isFalseIn model}
  override def toString: String = if (clauses.isEmpty) ""
    else (clauses.tail foldLeft clauses.head.toString) {case (string, clause) => s"($string & $clause)"}
}

case class Clause(literals: Set[Literal]) extends ComplexSentence(literals) {
  def isTrueIn(model: Model): Boolean = literals exists {_ isTrueIn model}
  def isFalseIn(model: Model): Boolean = literals forall {_ isFalseIn model}
  override def toString: String = if (literals.isEmpty) ""
    else (literals.tail foldLeft literals.head.toString) {case (string, literal) => s"($string | $literal)"}
}

sealed abstract class Literal(symbol: PropositionSymbol) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = Set(symbol)
}

case class PositiveLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Model): Boolean = symbol isTrueIn model
  def isFalseIn(model: Model): Boolean = symbol isFalseIn model
  override val toString: String = symbol.symbol
}

case class NegativeLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Model): Boolean = ¬(symbol) isTrueIn model
  def isFalseIn(model: Model): Boolean = ¬(symbol) isFalseIn model
  override val toString: String = s"(~$symbol.symbol)"
}

sealed abstract class HornClause(symbols: Set[PropositionSymbol]) extends ComplexSentence(symbols)

case class DefiniteClause(premises: Set[PropositionSymbol], conclusion: PropositionSymbol) extends HornClause(premises + conclusion) {
  import aima.core.logic.propositional.grammar._
  def isTrueIn(model: Model): Boolean = premises.reduce[Sentence] {_ ∧ _} ⇾ conclusion isTrueIn model
  def isFalseIn(model: Model): Boolean = premises.reduce[Sentence] {_ ∧ _} ⇾ conclusion isFalseIn model
  override def toString: String = (if (premises.isEmpty) ""
    else (premises.tail foldLeft premises.head.toString) {case (left, right) => s"($left & $right)"}) + s"=> $conclusion"
}

case class GoalClause(premises: Set[PropositionSymbol]) extends HornClause(premises) {
  import aima.core.logic.propositional.grammar._
  def isTrueIn(model: Model): Boolean = premises.reduce[Sentence] {_ ∧ _} ⇾ False isTrueIn model
  def isFalseIn(model: Model): Boolean = premises.reduce[Sentence] {_ ∧ _} ⇾ False isFalseIn model
  override def toString: String = if (premises.isEmpty) ""
    else (premises.tail foldLeft premises.head.toString) {case (left, right) => s"($left & $right)"}
}



