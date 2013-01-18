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

object collectClauseVariables {
  def apply(clause: Clause): Set[Variable] = {
    def fromTerm(term: Term): Set[Variable] = term match {
      case x: Variable => Set(x)
      case Function(_, terms) => terms flatMap fromTerm
      case ConstantFunction(_) => Set()
    }
    def fromLiteral(lit: Literal): Set[Variable] = lit.sentence.terms flatMap fromTerm
    clause match {
      case AtomicDefinite(lit) => fromLiteral(lit)
      case GeneralClause(lits) => lits flatMap fromLiteral
      case ImplicationDefinite(premises, conclusion) => (premises flatMap fromLiteral) ++ fromLiteral(conclusion)
      case DisjunctionDefinite(negLits, posLit) => (negLits flatMap fromLiteral) ++ fromLiteral(posLit)
    }
  }
}

object collectCNFVariables {
  def apply(cnf: CNFSentence): Set[Variable] = cnf.clauses flatMap collectClauseVariables.apply
}
