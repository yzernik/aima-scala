package aima.core.search.informed

import aima.core.search._
import scala.collection.mutable

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 92.<br>
 * <br>
 * Best-first search is an instance of the general TREE-SEARCH or GRAPH-SEARCH
 * algorithm in which a node is selected for expansion based on an evaluation
 * function, f(n). The evaluation function is construed as a cost estimate, so
 * the node with the lowest evaluation is expanded first. The implementation of
 * best-first graph search is identical to that for uniform-cost search (Figure
 * 3.14), except for the use of f instead of g to order the priority queue.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using graphSearch or treeSearch as an argument
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object bestFirstSearch {
  def apply[S, A](search: FrontierSearch[S, A])(order: Ordering[Node[S, A]]): Search[S, A, Seq[A]] =
    search(mutable.PriorityQueue()(order))
}
