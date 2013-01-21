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

import scala.annotation.tailrec
import aima.core.search._
import aima.core.search.informed.RBFSNode.createRBFSChildNode
import scala.Some

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.26, page
 * 99.<br>
 * <br>
 *
 * <pre>
 * function RECURSIVE-BEST-FIRST-SEARCH(problem) returns a solution, or failure
 * return RBFS(problem, MAKE-NODE(problem.INITIAL-STATE), infinity)
 *
 * function RBFS(problem, node, f_limit) returns a solution, or failure and a new f-cost limit
 * if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
 * successors &lt;- []
 * for each action in problem.ACTION(node.STATE) do
 * add CHILD-NODE(problem, node, action) into successors
 * if successors is empty then return failure, infinity
 * for each s in successors do // update f with value from previous search, if any
 * s.f &lt;- max(s.g + s.h, node.f)
 * repeat
 * best &lt;- the lowest f-value node in successors
 * if best.f &gt; f_limit then return failure, best.f
 * alternative &lt;- the second-lowest f-value among successors
 * result, best.f &lt;- RBFS(problem, best, min(f_limit, alternative))
 * if result != failure then return result
 * </pre>
 *
 * Figure 3.26 The algorithm for recursive best-first search.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
object recursiveBestFirstSearch {
  def apply[S, A](goalState: S, h: Heuristic[S]): Search[S, A, Seq[A]] = problem => {
    def rbfs(node: Node[S, A], fLimit: Double): (SearchResult[Seq[A]], Double) = {
      @tailrec
      def recur(successors: Seq[RBFSNode[S, A]]): (SearchResult[Seq[A]], Double) = {
        val best = successors.head
        if (best.f > fLimit) return (Failure, best.f)
        val (result, newF) = successors.tail.headOption match {
          case Some(alt) => rbfs(best, Math.min(fLimit, alt.f))
          case None => rbfs(best, Math.min(fLimit, best.f))
        }
        val newSuccessors = successors.tail :+ best.clone(newF) sortWith {(x, y) => x.f.compare(y.f) < 0}
        if (result != Failure) (result, 0) else recur(newSuccessors)
      }
      if (problem.goalTest(node.state))
        (Success(solutionActions(node)), 0)
      else
        problem.actionsFor(node.state) map {a => createRBFSChildNode(problem, node, a, h)} sortWith
          {(x, y) => x.f.compare(y.f) < 0} match {
          case Nil => (Failure, 0)
          case successors => recur(successors)
        }
    }
    rbfs(Node[S, A](problem.initialState, None, None, 0), Int.MaxValue)._1
  }
}

// Visible for testing purposes
private[search] final class RBFSNode[S, A] private(
    state: S,
    parent: Option[Node[S, A]],
    action: Option[A],
    pathCost: Double,
    val f: Double)
  extends Node[S, A](state, parent, action, pathCost) {
  override def canEqual(other: Any): Boolean = other.isInstanceOf[RBFSNode[S, A]]
  def clone(newF: Double): RBFSNode[S, A] = new RBFSNode[S, A](state, parent, action, pathCost, newF)
}

private[informed] object RBFSNode {
  def createRBFSChildNode[S, A](
      problem: Problem[S, A],
      parent: Node[S, A],
      action: A,
      h: Heuristic[S]): RBFSNode[S, A] = {
    val state = problem.result(parent.state, action)
    val pathCost = parent.pathCost + problem.stepCost(parent.state, action, state)
    val f = Math.max(pathCost + h(state), parent.pathCost + h(parent.state))
    new RBFSNode(state, Some(parent), Some(action), pathCost, f)
  }
}

