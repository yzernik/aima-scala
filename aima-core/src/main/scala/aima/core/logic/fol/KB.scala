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

import aima.core.logic.fol.Defaults._
import aima.core.logic.fol.DefiniteClause.definiteClauseToImplicationDefinite

final class KB(
  val sentences: Set[Sentence],
  val clauses: Set[Clause],
  val definiteClauses: Set[DefiniteClause],
  val predicates: Map[String, Set[Predicate]],
  val equalities: Set[TermEqual]) extends KnowledgeBase {self =>
  val implicationDefinites: Set[ImplicationDefinite] = definiteClauses map definiteClauseToImplicationDefinite

  def ask(sentence: Sentence): InferenceResult = ???

  protected def store(sentence: Sentence): KB = {
    val cnf = sentenceToCNF(sentence)
    if (!sentences(sentence) && !(cnf.clauses forall clauses)) {
      val newSentences = sentences + sentence
      val newClauses = clauses ++ cnf.clauses
      val cnfDefiniteClauses = cnf.clauses flatMap {
        case x: DefiniteClause => Some(x)
        case _ => None
      }
      val newDefiniteClauses = definiteClauses ++ cnfDefiniteClauses
      val newAtoms = cnfDefiniteClauses flatMap {
        case x: AtomicDefinite => Some(x)
        case _ => None
      }
      val newPredicates = predicates ++ (newAtoms flatMap {
        _.posLit.sentence match {
          case x: Predicate => Some(x.symbol → (predicates.getOrElse(x.symbol, Set()) + x))
          case x: TermEqual => None
        }
      })
      val newEqualities = equalities ++ (newAtoms flatMap {
        _.posLit.sentence match {
          case x: Predicate => None
          case x: TermEqual => Some(x)
        }
      })
      new KB(newSentences, newClauses, newDefiniteClauses, newPredicates, newEqualities)
    } else {
      self
    }
  }

  def fetch(literal: Literal): Set[Substitution] = literal.sentence match {
    case Predicate(symbol, terms) => predicates.get(symbol) match {
      case Some(matchingPredicates) => matchingPredicates flatMap {unify(literal.sentence, _)}
      case None => Set()
    }
    case x: TermEqual => equalities flatMap {unify(x, _)}
  }

  def fetch(literals: List[Literal]): Set[Substitution] =
    literals map fetch reduce {(subs1, subs2) => subs1 flatMap {sub1 => subs2 flatMap {unify.merge(sub1, _)}}}
}