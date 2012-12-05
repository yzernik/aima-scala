package aima.core.search.uninformed

import aima.core.search._
import aima.core.search.informed.bestFirstSearch

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.14, page
 * 84.<br>
 * <br>
 *
 * <pre>
 * function UNIFORM-COST-SEARCH(problem) returns a solution, or failure
 *   node &lt;- a node with STATE = problem.INITIAL-STATE, PATH-COST = 0
 *   frontier &lt;- a priority queue ordered by PATH-COST, with node as the only element
 *   explored &lt;- an empty set
 *   loop do
 *      if EMPTY?(frontier) then return failure
 *      node &lt;- POP(frontier) // chooses the lowest-cost node in frontier
 *      if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
 *      add node.STATE to explored
 *      for each action in problem.ACTIONS(node.STATE) do
 *          child &lt;- CHILD-NODE(problem, node, action)
 *          if child.STATE is not in explored or frontier then
 *             frontier &lt;- INSERT(child, frontier)
 *          else if child.STATE is in frontier with higher PATH-COST then
 *             replace that frontier node with child
 * </pre>
 *
 * Figure 3.14 Uniform-cost search on a graph. The algorithm is identical to the
 * general graph search algorithm in Figure 3.7, except for the use of a
 * priority queue and the addition of an extra check in case a shorter path to a
 * frontier state is discovered. The data structure for frontier needs to
 * support efficient membership testing, so it should combine the capabilities
 * of a priority queue and a hash table.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using graphSearch or
 * treeSearch as an argument
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object uniformCostSearch {
  def apply[S, A](search: FrontierSearch[S, A]): Search[S, A, Seq[A]] =
    bestFirstSearch(search)(Ordering.by(_.pathCost))
}
