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

package aima.core.logic

package object fol {
  type Substitution = Map[Variable, Term]
  // Produces a unique variable symbol from the given variable, thread-safe
  type StandardVariable = Variable => String
  // Produces a unique function given the set of terms, thread-safe
  type StandardFunction = Set[Term] => AFunction

  object Defaults extends FOLDefaults
}
