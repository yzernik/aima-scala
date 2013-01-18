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

package aima.core.search.informed

import aima.core.search._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 93.<br>
 * <br>
 * The most widely known form of best-first search is called A* Search
 * (pronounced "A-star search"). It evaluates nodes by combining g(n), the cost
 * to reach the node, and h(n), the cost to get from the node to the goal:<br>
 * f(n) = g(n) + h(n).<br>
 * <br>
 * Since g(n) gives the path cost from the start node to node n, and h(n) is the
 * estimated cost of the cheapest path from n to the goal, we have<br>
 * f(n) = estimated cost of the cheapest solution through n.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using graphSearch or treeSearch as an argument
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object aStarSearch {
  def apply[S, A](search: FrontierSearch[S, A])(h: Heuristic[S])(goal: S): Search[S, A, Seq[A]] =
    bestFirstSearch(search)(Ordering.by[Node[S, A], Double](node => h(node.state) + node.pathCost))
}
