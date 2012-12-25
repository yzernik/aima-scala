package aima.core.logic.fol

import aima.core.logic.fol.Connective._
import aima.core.logic.fol.grammar._

object substitute extends Substitute {
  def apply[T](θ: Substitution, α: T): T = {
    def substituteTerm(α: Term): Term = α match {
      case x: ConstantFunction => x
      case x: Variable => θ(x)
      case Function(symbol, terms) => Function(symbol, terms map substituteTerm)
    }
    def substituteAtomic(α: AtomicSentence): AtomicSentence = α match {
      case Predicate(symbol, terms) => Predicate(symbol, terms map substituteTerm)
      case TermEqual(left, right) => TermEqual(substituteTerm(left), substituteTerm(right))
    }
    def substituteLiteral(α: Literal): Literal = α match {
      case PositiveLiteral(x) => PositiveLiteral(substituteAtomic(x))
      case NegativeLiteral(x) => NegativeLiteral(substituteAtomic(x))
    }
    def substituteClause(α: Clause): Clause = GeneralClause(α.literals map substituteLiteral)
    def substituteComplex(α: ComplexSentence): ComplexSentence = α match {
      case ¬(x) => ¬(substitute(θ, x))
      case left ∧ right => substitute(θ, left) ∧ substitute(θ, right)
      case left ∨ right => substitute(θ, left) ∨ substitute(θ, right)
      case premise ⇾ conclusion => substitute(θ, premise) ⇾ substitute(θ, conclusion)
      case left ⇔ right => substitute(θ, left) ⇔ substitute(θ, right)
      case x: Clause => substituteClause(x)
      case CNFSentence(clauses) => CNFSentence(clauses map substituteClause)
    }
    α match {
      case x: AtomicSentence => substituteAtomic(x).asInstanceOf[T]
      case x: ComplexSentence => substituteComplex(x).asInstanceOf[T]
      case x: Term => substituteTerm(x).asInstanceOf[T]
    }
  }
}
