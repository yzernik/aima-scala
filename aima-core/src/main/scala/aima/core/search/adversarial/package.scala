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

package aima.core.search

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
package object adversarial {
  sealed case class Player(num: Int)
  type PlayerTurn[S] = S => Player
  type TerminalTest[S] = S => Boolean
  type Utility[S] = (S, Player) => Double
  type Cutoff[S] = (S, Int) => Boolean
  type AdversarialSearch[S, A] = S => SearchResult[A]
}
