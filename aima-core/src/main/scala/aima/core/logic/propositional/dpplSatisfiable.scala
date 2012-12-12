package aima.core.logic.propositional

class dpplSatisfiable {
  def apply(sentenceToCNF: SentenceToCNF, findPureSymbol: FindPureSymbol, findUnitClause: FindUnitClause)
    (sentence: Sentence): Boolean = {
    def dpll(clauses: Set[Clause], symbols: Set[PropositionSymbol], model: Model): Boolean = {
      if (clauses forall {_ isTrueIn model}) return true
      if (clauses exists {_ isFalseIn model}) return false
      findPureSymbol(clauses, symbols, model) match {
        case Some((symbol, value)) => dpll(clauses, symbols - symbol, model + (symbol → value))
        case None => findUnitClause(clauses, model) match {
          case Some((symbol, value)) => dpll(clauses, symbols - symbol, model + (symbol → value))
          case None => dpll(clauses, symbols.tail, model + (symbols.head → true)) ||
            dpll(clauses, symbols.tail, model + (symbols.head → false))
        }
      }
    }
    val clauses = sentenceToCNF(sentence).clauses
    dpll(clauses, clauses flatMap {_.symbols}, Map())
  }
}
