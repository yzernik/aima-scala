package aima.core.logic.propositional

case class CNFSentence(clauses: Set[Clause]) extends ComplexSentence(clauses) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = clauses reduce {∧(_, _) isTrueIn model}
}

case class Clause(literals: Set[Literal]) extends ComplexSentence(literals) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = literals reduce {∨(_, _) isTrueIn model}
}

sealed abstract class Literal(symbol: PropositionSymbol) extends Sentence {
  lazy val symbols: Set[PropositionSymbol] = Set(symbol)
}
case class PositiveLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = symbol isTrueIn model
}
case class NegativeLiteral(symbol: PropositionSymbol) extends Literal(symbol) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = ¬(symbol) isTrueIn model
}

sealed abstract class HornClause(symbols: Set[PropositionSymbol]) extends ComplexSentence(symbols)
case class DefiniteClause(premises: Set[PropositionSymbol], conclusion: PropositionSymbol) extends HornClause(premises + conclusion) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = ⇾(premises reduce {∧(_, _)}, conclusion) isTrueIn model
}
case class GoalClause(premises: Set[PropositionSymbol]) extends HornClause(premises) {
  def isTrueIn(model: Map[PropositionSymbol, Boolean]) = ⇾(premises reduce {∧(_, _)}, False) isTrueIn model
}

