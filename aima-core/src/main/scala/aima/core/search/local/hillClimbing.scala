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

package aima.core.search.local

import aima.core.search._
import scala.annotation.tailrec
import scala.Some

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.2, page
 * 122.<br>
 * <br>
 *
 * <pre>
 * function HILL-CLIMBING(problem) returns a state that is a local maximum
 *
 *   current &lt;- MAKE-NODE(problem.INITIAL-STATE)
 *   loop do
 *     neighbor &lt;- a highest-valued successor of current
 *     if neighbor.VALUE &lt;= current.VALUE then return current.STATE
 *     current &lt;- neighbor
 * </pre>
 *
 * Figure 4.2 The hill-climbing search algorithm, which is the most basic local
 * search technique. At each step the current node is replaced by the best
 * neighbor; in this version, that means the neighbor with the highest VALUE,
 * but if a heuristic cost estimate h is used, we would find the neighbor with
 * the lowest h.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using GraphFrontierExpander.nodeExpander or
 * TreeFrontierExpander.nodeExpander as an argument.
 *
 * <b>Note:</b> Returns a tuple of (local maximum state, sequence of actions from start state to local maximum state)
 * . Notice that the sequence of actions is not necessarily optimal in path cost, however an optimal sequence can be
 * found using any of the algorithms in uninformed or informed search with the local maximum as the goal state.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
object hillClimbing {
  def apply[S, A](nodeExpander: NodeExpander[S, A])
    (problem: Problem[S, A], h: Heuristic[S]): Search[S, A, (S, Seq[A])] = problem => {
    def sortedNeighbors = (node: Node[S, A]) => nodeExpander(problem, node) sortBy { n => h(n.state) }
    @tailrec
    def recur(current: Node[S, A], neighbors: Traversable[Node[S, A]]): SearchResult[(S, Seq[A])] =
      neighbors.headOption match {
        case Some(neighbor) if h(neighbor.state) <= h(current.state) => Success(current.state, solutionActions(current))
        case Some(neighbor) => recur(neighbor, sortedNeighbors(neighbor))
        case None => Failure
      }
    val startNode = Node[S, A](problem.initialState, None, None, 0)
    recur(startNode, sortedNeighbors(startNode))
  }
}
