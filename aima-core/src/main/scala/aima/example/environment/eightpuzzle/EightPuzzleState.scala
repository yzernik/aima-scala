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

package aima.example.environment.eightpuzzle

import scala.collection.SeqProxy

case class EightPuzzleState private[eightpuzzle](board: IndexedSeq[IndexedSeq[Int]]) extends SeqProxy[IndexedSeq[Int]] {
  require(board.size == 3 && (board forall {_.size == 3}), "Board was not 3 x 3")
  require((0 to 8) forall {tile => board exists {_ contains tile}},
    s"Board did not contain every number between 0 and 8: $board")

  val complete: Boolean = board.flatten == (0 to 8)

  def self: IndexedSeq[IndexedSeq[Int]] = board

  def indexOf(tile: Int): (Int, Int) = {
    require(0 to 8 contains tile, s"Tile $tile is not between 0 and 8")
    val ySeq = (board find {_ contains tile}).get
    (board indexOf ySeq, ySeq indexOf tile)
  }

  def canMoveGap(direction: MoveGap): Boolean = direction match {
    case Left  => indexOf(0)._1 != 0
    case Right => indexOf(0)._1 != 2
    case Up    => indexOf(0)._2 != 0
    case Down  => indexOf(0)._2 != 2
  }

  def moveGap(direction: MoveGap): EightPuzzleState = {
    def swap(first: (Int, Int), second: (Int, Int)): EightPuzzleState = {
      def updateValue(board: IndexedSeq[IndexedSeq[Int]], index: (Int, Int), value: Int): IndexedSeq[IndexedSeq[Int]] =
        board updated (index._1, board(index._1) updated (index._2, value))
      val firstSwapped = updateValue(board, first, board(second._1)(second._2))
      val secondSwapped = updateValue(firstSwapped, second, board(first._1)(first._2))
      EightPuzzleState(secondSwapped)
    }

    val gap = indexOf(0)
    direction match {
      case Left if canMoveGap(Left)   => swap(gap, (gap._1 - 1, gap._2))
      case Right if canMoveGap(Right) => swap(gap, (gap._1 + 1, gap._2))
      case Up if canMoveGap(Up)       => swap(gap, (gap._1, gap._2 - 1))
      case Down if canMoveGap(Down)   => swap(gap, (gap._1, gap._2 + 1))
      case _ => throw new IllegalArgumentException(s"Cannot move gap $direction in board $board")
    }
  }
}
