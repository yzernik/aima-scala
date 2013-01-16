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

import aima.core.search._
import scala.annotation.tailrec

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.18, page
 * 89.<br>
 * <br>
 *
 * <pre>
 * function ITERATIVE-DEEPENING-SEARCH(problem) returns a solution, or failure
 *   for depth = 0 to infinity  do
 *     result &lt;- DEPTH-LIMITED-SEARCH(problem, depth)
 *     if result != cutoff then return result
 * </pre>
 *
 * Figure 3.18 The iterative deepening search algorithm, which repeatedly
 * applies depth-limited search with increasing limits. It terminates when a
 * solution is found or if the depth- limited search returns failure, meaning
 * that no solution exists.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using TreeFrontierExpander() or
 * GraphNodeExpander() as an argument, respectively
 *
 * @author Alex DiCarlo
 */
object iterativeDeepeningSearch {
  def apply[S, A](nodeExpander: NodeExpander[S, A])(limit: Int): Search[S, A, Seq[A]] = problem => {
    val myDepthLimitedSearch = depthLimitedSearch(nodeExpander) _
    @tailrec
    def recur(depths: Stream[Int]): SearchResult[Seq[A]] =
      myDepthLimitedSearch(depths.head)(problem) match {
        case success@Success(_) => success
        case _ => recur(depths.tail)
      }
    recur(Stream.from(0))
  }
}
