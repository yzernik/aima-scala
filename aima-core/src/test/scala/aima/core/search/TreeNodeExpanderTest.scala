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

class TreeNodeExpanderTest extends SearchFixture {
  feature("TreeNodeExpander") {
    scenario("TreeNodeExpander expands all children of node") {
      Given("a TreeNodeExpander")
      val treeNodeExpander = TreeNodeExpander[String, StringAction]()
      And("a node")
      When("the node is expanded")
      val (result, _ ) = treeNodeExpander(stringProblem, initialNode)
      Then("the result is all of the state's actions as children")
      result should be(createChildren(initialNode))
    }
  }
}
