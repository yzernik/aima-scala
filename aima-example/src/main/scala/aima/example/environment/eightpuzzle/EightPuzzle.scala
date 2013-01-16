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

package aima.example.environment.eightpuzzle

import aima.core.search.Problem

object EightPuzzle {
  def apply(initialState: IndexedSeq[IndexedSeq[Int]]): Problem[EightPuzzleState, MoveGap] = {
    def actions(state: EightPuzzleState): Seq[MoveGap] =
      Seq(Left, Right, Up, Down) filter {direction => state canMoveGap direction}
    def result(state: EightPuzzleState, direction: MoveGap): EightPuzzleState = state moveGap direction
    def goalTest(state: EightPuzzleState): Boolean = state.complete
    def stepCost(prev: EightPuzzleState, direction: MoveGap, curr: EightPuzzleState): Int = 1

    Problem(EightPuzzleState(initialState), actions, result, goalTest, stepCost)
  }
}
