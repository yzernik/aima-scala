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
