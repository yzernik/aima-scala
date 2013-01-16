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

package aima.core.search.informed

import aima.core.search._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 92.<br>
 * <br>
 * Greedy best-first search tries to expand the node that is closest to the
 * goal, on the grounds that this is likely to lead to a solution quickly. Thus,
 * it evaluates nodes by using just the heuristic function; that is, f(n) = h(n)
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using graphSearch or treeSearch as an argument
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object greedyBestFirstSearch {
  def apply[S, A](search: FrontierSearch[S, A])(h: Heuristic[S])(goal: S): Search[S, A, Seq[A]] =
    bestFirstSearch(search)(Ordering.by[Node[S, A], Double](node => h(node.state)))
}
