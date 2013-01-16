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

package aima.core.search

import aima.core.search.Node.createChildNode

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object GraphFrontierExpander {
  def apply[S, A](): FrontierExpander[S, A] = {
    val myNodeExpander = nodeExpander[S, A]()
    (problem, node, frontier) => (frontier filterNot {_ == node}) ++ myNodeExpander(problem, node)
  }

  def nodeExpander[S, A](): NodeExpander[S, A] = {
    var explored = Set[Node[S, A]]()
    (problem, node) => {
      explored += node
      problem.actions(node.state) map {createChildNode(problem, node, _)} filterNot explored
    }
  }
}
