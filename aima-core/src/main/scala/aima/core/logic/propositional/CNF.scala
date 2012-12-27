package aima.core.logic.propositional

import aima.core.logic.propositional.grammar._

case class CNFSentence(clauses: Set[Clause]) extends ComplexSentence(clauses) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]): Boolean = clauses forall {_ isTrueIn model}
  def isFalseIn(model: Map[PropositionSymbol, Boolean]): Boolean = clauses exists {_ isFalseIn model}
  override def toString: String = if (clauses.isEmpty) ""
    else (clauses.tail foldLeft clauses.head.toString){case (string, clause) => s"($string & $clause)"}
}

case class Clause(literals: Set[Literal]) extends ComplexSentence(literals) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = literals exists {_ isTrueIn model}
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = literals forall {_ isFalseIn model}
  override def toString: String = if (literals.isEmpty) ""
    else (literals.tail foldLeft literals.head.toString){case (string, literal) => s"($string | $literal)"}
}

sealed abstract class Literal(symbol: PropositionSymbol) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = Set(symbol)
}
case class PositiveLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = symbol isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = symbol isFalseIn model
  override val toString: String = symbol.symbol
}
case class NegativeLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = ¬(symbol) isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = ¬(symbol) isFalseIn model
  override val toString: String = s"(~$symbol.symbol)"
}

sealed abstract class HornClause(symbols: Set[PropositionSymbol]) extends ComplexSentence(symbols)
case class DefiniteClause(premises: Set[PropositionSymbol], conclusion: PropositionSymbol) extends HornClause(premises + conclusion) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ conclusion isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ conclusion isFalseIn model
  override def toString: String = (if (premises.isEmpty) ""
    else (premises.tail foldLeft premises.head.toString){case (left, right) => s"($left & $right)"}) + s"=> $conclusion"
}
case class GoalClause(premises: Set[PropositionSymbol]) extends HornClause(premises) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ False isTrueIn model
  def isFalseIn(model: Map[PropositionSymbol, Boolean]) = premises.reduce[Sentence]{_ ∧ _} ⇾ False isFalseIn model
  override def toString: String = if (premises.isEmpty) ""
    else (premises.tail foldLeft premises.head.toString){case (left, right) => s"($left & $right)"}
}

