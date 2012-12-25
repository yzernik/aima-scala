package aima.core.logic.fol

object collectVariables {
  def apply(cnf: CNFSentence): Set[Variable] = {
    def fromTerm(term: Term): Set[Variable] = term match {
      case x: Variable => Set(x)
      case Function(_, terms) => terms flatMap fromTerm
      case ConstantFunction(_) => Set()
    }
    def fromLiteral(lit: Literal): Set[Variable] = lit.sentence.terms flatMap fromTerm
    cnf.clauses flatMap {
      case AtomicDefinite(lit) => fromLiteral(lit)
      case GeneralClause(lits) => lits flatMap fromLiteral
      case ImplicationDefinite(premises, conclusion) => (premises flatMap fromLiteral) ++ fromLiteral(conclusion)
      case DisjunctionDefinite(negLits, posLit) => (negLits flatMap fromLiteral) ++ fromLiteral(posLit)
    }
  }
}
