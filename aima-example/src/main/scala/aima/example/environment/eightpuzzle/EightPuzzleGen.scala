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

import org.scalacheck.Gen

trait EightPuzzleGen {
  private def generateSeq(): Seq[Int] = util.Random.shuffle((0 to 8).to[Seq])
  private def generateBoard(seq: Seq[Int]): IndexedSeq[IndexedSeq[Int]] = seq.to[IndexedSeq].grouped(3).to[IndexedSeq]

  val boards: Gen[IndexedSeq[IndexedSeq[Int]]] = for {
    randomList <- Gen.resultOf((any: Int) => generateSeq())
  } yield generateBoard(randomList)

  val eightPuzzles: Gen[EightPuzzleState] = for {
    board <- boards
  } yield EightPuzzleState(board)

  val tiles: Gen[Int] = for {
    tile <- Gen.choose(0, 8)
  } yield tile

  val moves: Gen[MoveGap with Product] = for {
    move <- Gen.oneOf(Seq(Left, Right, Up, Down))
  } yield move
}
