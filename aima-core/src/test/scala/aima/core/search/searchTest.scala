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

// TODO: Commented out until ScalaMock is issue is fixed
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{GivenWhenThen, FeatureSpec}

class searchTest extends FeatureSpec with
                         GivenWhenThen with
                         ShouldMatchers with
                         GeneratorDrivenPropertyChecks with
                         MockFactory {
  feature("Searching with a node expander") {
    scenario("Searching with a node expander and frontier") {
      Given("a frontier and a node expander")
      val (frontier, nodeExpander) = nodeExpanderAndFrontier(success = true)
      And("a problem")
      When("#search() is called with the node expander, a frontier, and a problem")
      search(frontier, nodeExpander)(stringProblem)
      Then("the node expander is used to expand nodes")
      And("the children are added to the frontier")
      And("the expanded node is removed")
    }

    scenario("Successfully solving a Problem through search") {
      Given("a frontier and a node expander")
      val (frontier, nodeExpander) = nodeExpanderAndFrontier(success = true)
      And("a problem")
      When("#search() is called with the node expander, a frontier, and a problem")
      val result = search(frontier, nodeExpander)(stringProblem)
      Then("the result is Success(Seq(AddC))")
      result should be('defined)
      result.get should equal(Seq(AddC))
    }

    scenario("Attempting to solve a Problem through search that cannot be solved") {
      Given("a frontier and a node expander")
      val (frontier, nodeExpander) = nodeExpanderAndFrontier(success = false)
      And("an unsolveable problem")
      When("#search() is called with the node expander, a frontier, and a problem")
      And("the frontier is empty because the node expander cannot expand any more nodes")
      val result = search(frontier, nodeExpander)(stringProblem)
      Then("the result is Failure")
      result should be(Failure)
    }
  }

  private abstract class StringAction(val addition: String)
  private case object AddA extends StringAction("A")
  private case object AddB extends StringAction("B")
  private case object AddC extends StringAction("C")

  private def actions(state: String): Seq[StringAction] =
    if (state.length < 2) Seq(AddA, AddB, AddC) else Seq()
  private def result(state: String, action: StringAction): String = state + action.addition
  private def goalTest(state: String): Boolean = state == "C"
  private def stepCost(s: String, action: StringAction, statePrime: String): Int = 1
  private val stringProblem = Problem("", actions, result, goalTest, stepCost)

  private def childNode(parent: Node[String, StringAction], action: StringAction): Node[String, StringAction] =
    Node.createChildNode(stringProblem, parent, action)
  private def createChildren(parent: Node[String, StringAction]): Seq[Node[String, StringAction]] =
    Seq(childNode(parent, AddA), childNode(parent, AddB), childNode(parent, AddC))

  private def nodeExpanderAndFrontier(
    success: Boolean): (Frontier[String, StringAction], NodeExpander[String, StringAction]) = {
    val frontier = mock[Frontier[String, StringAction]]
    val nodeExpander = mock[NodeExpander[String, StringAction]]
    inSequence {
      val initialNode = Node[String, StringAction](stringProblem.initialState, None, None, 0.0)
      frontier.expects('add)(initialNode).returning(frontier)
//      (frontier.add _).expects(initialNode).returning(frontier)

      def iteration(node: Node[String, StringAction]): Seq[Node[String, StringAction]] = {
        frontier.expects('first)().returning(Some(node))
//        (frontier.first _).expects().returning(Some(node))
        val children = createChildren(node)
        nodeExpander.expects('apply)(stringProblem, node).returning(children, nodeExpander)
//        (nodeExpander.apply _).expects(stringProblem, node).returning(children, nodeExpander)
        frontier.expects('rest)().returning(frontier)
//        (frontier.rest _).expects().returning(frontier)
        frontier.expects('addAll)().returning(frontier)
//        (frontier.addAll _).expects(children).returning(frontier)
        children
      }

      val children = iteration(initialNode)
      iteration(children(0))
      iteration(children(1))
      if (success)
        frontier.expects('first)().returning(Some(children(2)))
//        (frontier.first _).expects().returning(Some(children(2)))
      else
        frontier.expects('first)().returning(None)
//        (frontier.first _).expects().returning(None)
    }
    (frontier, nodeExpander)
  }
}
