/*
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

object ttEntails {
  def apply(kb: KnowledgeBase[_], α: Sentence): Boolean = {
    def ttCheckAll(symbols: List[PropositionSymbol], model: Map[PropositionSymbol,
      Boolean]): Boolean = symbols match {
      case symbol :: tail => ttCheckAll(tail, model + (symbol → true)) && ttCheckAll(tail, model + (symbol → false))
      case Nil => if (kb.asSentence isTrueIn model) α isTrueIn model else true
    }
    ttCheckAll((kb.asSentence.symbols ++ α.symbols).to[List], Map())
  }
}
