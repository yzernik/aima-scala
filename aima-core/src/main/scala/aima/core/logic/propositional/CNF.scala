package aima.core.logic.propositional

import aima.core.logic.propositional.grammar._

case class CNFSentence(clauses: Set[Clause]) extends ComplexSentence(clauses) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = clauses forall {_ isTrueIn model}
  def isFalseIn(model: Map[PropositionSymbol, Boolean]): Boolean = clauses exists {_ isFalseIn model}
}

case class Clause(literals: Set[Literal]) extends ComplexSentence(literals) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = literals exists {_ isTrueIn model}
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = literals forall {_ isFalseIn model}
}

sealed abstract class Literal(symbol: PropositionSymbol) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = Set(symbol)
}
case class PositiveLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = symbol isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = symbol isFalseIn model
}
case class NegativeLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = ¬(symbol) isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = ¬(symbol) isFalseIn model
}

sealed abstract class HornClause(symbols: Set[PropositionSymbol]) extends ComplexSentence(symbols)
case class DefiniteClause(premises: Set[PropositionSymbol], conclusion: PropositionSymbol) extends HornClause(premises + conclusion) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ conclusion isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ conclusion isFalseIn model
}
case class GoalClause(premises: Set[PropositionSymbol]) extends HornClause(premises) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ False isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ False isFalseIn model
}

