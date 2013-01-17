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

package aima.core.search.uninformed

import org.scalatest.{GivenWhenThen, FeatureSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import aima.example.environment.eightpuzzle.{MoveGap, EightPuzzle, EightPuzzleGen, EightPuzzleState}
import aima.core.search.{treeSearch, graphSearch}

class breadthFirstSearchTest extends FeatureSpec with
                                     GivenWhenThen with
                                     ShouldMatchers with
                                     GeneratorDrivenPropertyChecks with
                                     EightPuzzleGen {
  feature("BFS can solve a graph search problem") {
    scenario("Eight puzzle graph search") {
      Given("an eight puzzle problem")
      val puzzle = EightPuzzle(IndexedSeq(IndexedSeq(4, 0, 2), IndexedSeq(1, 3, 7), IndexedSeq(6, 8, 5)))
      When("#breadthFirstSearch is called with graphSearch")
      val resultActions = breadthFirstSearch[EightPuzzleState, MoveGap](graphSearch.apply)(puzzle)
      Then("a solution is returned")
      checkSolution(puzzle, resultActions)
    }

    scenario("Eight puzzle tree search") {
      Given("an eight puzzle problem")
      val puzzle = EightPuzzle(IndexedSeq(IndexedSeq(3, 4, 1), IndexedSeq(7, 6, 2), IndexedSeq(0, 8, 5)))
      When("#breadthFirstSearch is called with treeSearch")
      val resultActions = breadthFirstSearch[EightPuzzleState, MoveGap](treeSearch.apply)(puzzle)
      Then("a solution is returned")
      checkSolution(puzzle, resultActions)
    }
  }
}
