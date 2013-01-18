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

class GraphNodeExpanderTest extends SearchFixture {
  feature("GraphNodeExpander") {
    scenario("GraphNodeExpander uses TreeNodeExpander to expand node") {
      Given("a TreeNodeExpander")
      val treeNodeExpander = mock[NodeExpander[String, StringAction]]
      val children = createChildren(initialNode)
      (treeNodeExpander.apply _).expects(stringProblem, initialNode).returning(children, treeNodeExpander)
      And("a GraphNodeExpander using that TreeNodeExpander")
      val graphNodeExpander = GraphNodeExpander(treeNodeExpander)
      And("a node")
      When("the GraphNodeExpander is used to expand the node")
      val (result, _) = graphNodeExpander(stringProblem, initialNode)
      Then("the TreeNodeExpander was used to expand the node")
      result should be(children)
    }

    scenario("GraphNodeExpander filters previously expanded nodes") {
      Given("a TreeNodeExpander")
      val treeNodeExpander = mock[NodeExpander[String, StringAction]]
      val children = createChildren(initialNode)
      (treeNodeExpander.apply _).expects(stringProblem, initialNode).returning(children, treeNodeExpander).twice()
      And("a GraphNodeExpander using that TreeNodeExpander that has already expanded some nodes")
      val (_, graphNodeExpander) = GraphNodeExpander(treeNodeExpander)(stringProblem, initialNode)
      And("a node")
      When("the GraphNodeExpander is used to expand the node")
      val (result, _) = graphNodeExpander(stringProblem, initialNode)
      Then("the children that were previously explored are not returned")
      children foreach {child => result should not (contain(child))}
    }
  }
}
