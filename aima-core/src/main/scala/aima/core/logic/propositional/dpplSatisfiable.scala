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
