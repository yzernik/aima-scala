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
 * @author Alex DiCarlo
 */
final class TreeNodeExpander[S, A] private() extends NodeExpander[S, A] {
  def apply(problem: Problem[S, A], node: Node[S, A]): (Seq[Node[S, A]], NodeExpander[S, A]) =
    (problem actionsFor node.state map {createChildNode(problem, node, _)}, this)
}

object TreeNodeExpander {
  def apply[S, A](): TreeNodeExpander[S, A] = new TreeNodeExpander
}
