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

import org.scalatest.{GivenWhenThen, FeatureSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class EightPuzzleStateTest extends FeatureSpec with
                                   GivenWhenThen with
                                   ShouldMatchers with
                                   GeneratorDrivenPropertyChecks with
                                   EightPuzzleGen {
  def invalidNumber(tile: Int): Boolean = !(0 to 8 contains tile)

  feature("EightPuzzleState only allows correct 3x3 boards") {
    scenario("Randomly generated incorrectly sized boards") {
      Given("An incorrectly sized board")
      val incorrectBoard = for {
        xDimension <- Gen.posNum[Int]
        yDimension <- Gen.posNum[Int]
      } yield IndexedSeq.fill(xDimension)(IndexedSeq.fill(yDimension)(util.Random.nextInt(9)))

      When("I construct an EightPuzzleState with it")

      Then("An IllegalArgumentException is thrown")
      forAll(incorrectBoard) {board =>
        intercept[IllegalArgumentException] {
          EightPuzzleState(board)
        }
      }
    }

    scenario("Randomly generated correctly sized boards with incorrect elements") {
      Given("A correctly sized board with incorrect elements")
      val row = for {
        row <- Gen.containerOfN[List, Int](3, Gen.chooseNum(Int.MinValue, Int.MaxValue))
      } yield row.to[IndexedSeq]

      val invalidBoard = for {
        board <- Gen.containerOfN[List, IndexedSeq[Int]](3, row) suchThat {board =>
          board exists {_ exists {invalidNumber(_)}}
        }
      } yield board.to[IndexedSeq]

      When("I construct an EightPuzzleState with it")

      Then("Then an IllegalArgumentException is thrown")
      forAll(invalidBoard) {board =>
        intercept[IllegalArgumentException] {
          EightPuzzleState(board)
        }
      }
    }

    scenario("Correctly sized and tiled boards") {
      Given("A correctly sized board with correct elements")
      When("I construct an EightPuzzleState with it")
      Then("EightPuzzleState constructs successfully")
      forAll(boards) {board =>
        EightPuzzleState(board)
      }
    }
  }

  feature("EightPuzzleState lets you find the position of a tile") {
    scenario("Incorrect tile number") {
      Given("An EightPuzzleState and an incorrect tile")
      val incorrectTiles = for {
        tile <- Gen.oneOf(Gen.choose(Int.MinValue, -1), Gen.choose(9, Int.MaxValue))
      } yield tile

      When("EightPuzzleState#indexOf is called on the tile")
      Then("An IllegalArgumentException is thrown")
      forAll(incorrectTiles, eightPuzzles) {(tile, eightPuzzle) =>
        intercept[IllegalArgumentException] {
          eightPuzzle indexOf tile
        }
      }
    }

    scenario("Correct tile number") {
      Given("An EightPuzzleState and a correct tile")
      When("EightPuzzleState#indexOf is called on the tile")
      Then("The value at that position is equal to the tile")
      forAll(tiles, eightPuzzles) {(tile, eightPuzzle) =>
        val index = eightPuzzle indexOf tile
        eightPuzzle(index._1)(index._2) should equal(tile)
      }
    }
  }

  feature("EightPuzzleState lets you determine if you can move the gap") {
    scenario("Left") {
      Given("An EightPuzzleState")
      When("EightPuzzleState#canMove(Left) is called")
      Then("The result is true if the gap is not in the first column")
      forAll(eightPuzzles) {eightPuzzle =>
        if ((eightPuzzle indexOf 0)._1 > 0)
          eightPuzzle canMoveGap Left should be(true)
        else
          eightPuzzle canMoveGap Left should be(false)
      }
    }

    scenario("Right") {
      Given("An EightPuzzleState")
      When("EightPuzzleState#canMove(Right) is called")
      Then("The result is true if the gap is not in the third column")
      forAll(eightPuzzles) {eightPuzzle =>
        if ((eightPuzzle indexOf 0)._1 < 2)
          eightPuzzle canMoveGap Right should be(true)
        else
          eightPuzzle canMoveGap Right should be(false)
      }
    }

    scenario("Up") {
      Given("An EightPuzzleState")
      When("EightPuzzleState#canMove(Up) is called")
      Then("The result is true if the gap is not in the first row")
      forAll(eightPuzzles) {eightPuzzle =>
        if ((eightPuzzle indexOf 0)._2 > 0)
          eightPuzzle canMoveGap Up should be(true)
        else
          eightPuzzle canMoveGap Up should be(false)
      }
    }

    scenario("Down") {
      Given("An EightPuzzleState")
      When("EightPuzzleState#canMove(Down) is called")
      Then("The result is true if the gap is not in the third row")
      forAll(eightPuzzles) {eightPuzzle =>
        if ((eightPuzzle indexOf 0)._2 < 2)
          eightPuzzle canMoveGap Down should be(true)
        else
          eightPuzzle canMoveGap Down should be(false)
      }
    }
  }

  feature("EightPuzzleState lets you move the gap") {
    scenario("Left") {
      Given("An EightPuzzleState where the gap can be moved left")
      When("EightPuzzleState#moveGap(Left) is called")
      Then("The result is an EightPuzzleState with the gap swapped with the tile to the left")
      forAll(eightPuzzles) {eightPuzzle =>
        whenever(eightPuzzle canMoveGap Left) {
          val newPuzzle = eightPuzzle moveGap Left
          val (oldGapX, oldGapY) = eightPuzzle indexOf 0
          val (newGapX, newGapY) = newPuzzle indexOf 0
          eightPuzzle(newGapX)(newGapY) should equal(newPuzzle(oldGapX)(oldGapY))
          newGapX should be(oldGapX - 1)
          newGapY should be(oldGapY)
        }
      }
    }

    scenario("Right") {
      Given("An EightPuzzleState where the gap can be moved right")
      When("EightPuzzleState#moveGap(Right) is called")
      Then("The result is an EightPuzzleState with the gap swapped with the tile to the right")
      forAll(eightPuzzles) {eightPuzzle =>
        whenever(eightPuzzle canMoveGap Right) {
          val newPuzzle = eightPuzzle moveGap Right
          val (oldGapX, oldGapY) = eightPuzzle indexOf 0
          val (newGapX, newGapY) = newPuzzle indexOf 0
          eightPuzzle(newGapX)(newGapY) should equal(newPuzzle(oldGapX)(oldGapY))
          newGapX should be(oldGapX + 1)
          newGapY should be(oldGapY)
        }
      }
    }

    scenario("Up") {
      Given("An EightPuzzleState where the gap can be moved up")
      When("EightPuzzleState#moveGap(Up) is called")
      Then("The result is an EightPuzzleState with the gap swapped with the tile to the up")
      forAll(eightPuzzles) {eightPuzzle =>
        whenever(eightPuzzle canMoveGap Up) {
          val newPuzzle = eightPuzzle moveGap Up
          val (oldGapX, oldGapY) = eightPuzzle indexOf 0
          val (newGapX, newGapY) = newPuzzle indexOf 0
          eightPuzzle(newGapX)(newGapY) should equal(newPuzzle(oldGapX)(oldGapY))
          newGapX should be(oldGapX)
          newGapY should be(oldGapY - 1)
        }
      }
    }

    scenario("Down") {
      Given("An EightPuzzleState where the gap can be moved down")
      When("EightPuzzleState#moveGap(Down) is called")
      Then("The result is an EightPuzzleState with the gap swapped with the tile to the down")
      forAll(eightPuzzles) {eightPuzzle =>
        whenever(eightPuzzle canMoveGap Down) {
          val newPuzzle = eightPuzzle moveGap Down
          val (oldGapX, oldGapY) = eightPuzzle indexOf 0
          val (newGapX, newGapY) = newPuzzle indexOf 0
          eightPuzzle(newGapX)(newGapY) should equal(newPuzzle(oldGapX)(oldGapY))
          newGapX should be(oldGapX)
          newGapY should be(oldGapY + 1)
        }
      }
    }

    scenario("Invalid Move") {
      Given("An EightPuzzleState and a move which can't be completed")
      When("EightPuzzleState#moveGap(move) is called")
      Then("IllegalArgumentException is thrown")
      forAll(eightPuzzles, moves) {(eightPuzzle, move) =>
        whenever(!(eightPuzzle canMoveGap move)) {
          intercept[IllegalArgumentException] {
            eightPuzzle moveGap move
          }
        }
      }
    }
  }

  feature("Complete Board") {
    scenario("Complete eight puzzle board") {
      Given("A complete EightPuzzleState")
      val puzzle = EightPuzzleState(completeBoard)
      Then("EightPuzzleState#completed is true")
      puzzle.complete should be(true)
    }

    scenario("Incomplete eight puzzle board") {
      Given("An incomplete EightPuzzleState")
      Then("EightPuzzleState#completed is false")
      forAll(eightPuzzles) {eightPuzzle =>
        whenever(eightPuzzle.board != completeBoard) {
          eightPuzzle.complete should be(false)
        }
      }
    }
  }

}
