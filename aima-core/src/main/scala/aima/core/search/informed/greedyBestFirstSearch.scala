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
