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

package aima.core.logic.fol

trait KnowledgeBase {
  def tell[T <: Sentence](sentence: T): KnowledgeBase = store(sentence)
  def tell[T <: Sentence](sentences: Set[T]): KnowledgeBase = (sentences foldLeft this) {case (kb, s) => kb.tell(s)}
  protected def store(sentence: Sentence): KnowledgeBase
  def ask(sentence: Sentence): InferenceResult
  def fetch(literal: Literal): Set[Substitution]
  def fetch(literals: List[Literal]): Set[Substitution]
  def implicationDefinites: Set[ImplicationDefinite]
}
