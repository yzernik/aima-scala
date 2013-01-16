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

import scala.annotation.tailrec
import scala.Some

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object search {
  def apply[S, A](frontier: Frontier[S, A], frontierExpander: FrontierExpander[S, A]): Search[S, A, Seq[A]] =
    problem => {
      @tailrec
      def recur(frontier: Frontier[S, A]): SearchResult[Seq[A]] = frontier.headOption match {
        case None => Failure
        case Some(node) if problem.goalTest(node.state) => Success(solutionActions(node))
        case Some(node) => recur(frontierExpander(problem, node, frontier))
      }
      recur(frontier.take(0) ++ Traversable(Node[S, A](problem.initialState, None, None, 0)))
    }
}
