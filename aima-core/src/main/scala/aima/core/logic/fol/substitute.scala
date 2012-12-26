package aima.core.logic.fol

import aima.core.logic.fol.Connective._
import aima.core.logic.fol.grammar._

object substitute {
  def term[A <: Term](θ: Substitution, α: A): A = α match {
    case x: ConstantFunction => x.asInstanceOf[A]
    case x: Variable => θ(x).asInstanceOf[A]
    case Function(symbol, terms) => Function(symbol, terms map {substitute.term(θ, _)}).asInstanceOf[A]
  }

  def apply[A <: Sentence](θ: Substitution, α: A): A = {
    def substituteAtomic(α: AtomicSentence): AtomicSentence = α match {
      case Predicate(symbol, terms) => Predicate(symbol, terms map {substitute.term(θ, _)})
      case TermEqual(left, right) => TermEqual(substitute.term(θ, left), substitute.term(θ, right))
      case x: Literal => substituteLiteral(x)
    }
    def substituteLiteral[B <: Literal](α: B): B = α match {
      case PositiveLiteral(x) => PositiveLiteral(substituteAtomic(x)).asInstanceOf[B]
      case NegativeLiteral(x) => NegativeLiteral(substituteAtomic(x)).asInstanceOf[B]
    }
    def substituteClause(α: Clause): Clause = GeneralClause(α.literals map substituteLiteral)
    def substituteComplex(α: ComplexSentence): ComplexSentence = α match {
      case ¬(x) => ¬(substitute(θ, x))
      case left ∧ right => substitute(θ, left) ∧ substitute(θ, right)
      case left ∨ right => substitute(θ, left) ∨ substitute(θ, right)
      case premise ⇾ conclusion => substitute(θ, premise) ⇾ substitute(θ, conclusion)
      case left ⇔ right => substitute(θ, left) ⇔ substitute(θ, right)
      case GeneralClause(lits) => GeneralClause(lits map substituteLiteral)
      case AtomicDefinite(lit) => AtomicDefinite(substituteLiteral(lit))
      case ImplicationDefinite(premises, conclusion) =>
        ImplicationDefinite(premises map substituteLiteral, substituteLiteral(conclusion))
      case DisjunctionDefinite(negLits, posLit) =>
        DisjunctionDefinite(negLits map substituteLiteral, substituteLiteral(posLit))
      case CNFSentence(clauses) => CNFSentence(clauses map substituteClause)
    }
    α match {
      case x: AtomicSentence => substituteAtomic(x).asInstanceOf[A]
      case x: ComplexSentence => substituteComplex(x).asInstanceOf[A]
    }
  }
}
