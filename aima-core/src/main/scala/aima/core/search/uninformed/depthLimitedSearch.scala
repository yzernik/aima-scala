package aima.core.search.uninformed

import aima.core.search._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.17, page
 * 88.<br>
 * <br>
 *
 * <pre>
 * function DEPTH-LIMITED-SEARCH(problem, limit) returns a solution, or failure/cutoff
 *   return RECURSIVE-DLS(MAKE-NODE(problem.INITIAL-STATE), problem, limit)
 *
 * function RECURSIVE-DLS(node, problem, limit) returns a solution, or failure/cutoff
 *   if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
 *   else if limit = 0 then return cutoff
 *   else
 *       cutoff_occurred? &lt;- false
 *       for each action in problem.ACTIONS(node.STATE) do
 *           child &lt;- CHILD-NODE(problem, node, action)
 *           result &lt;- RECURSIVE-DLS(child, problem, limit - 1)
 *           if result = cutoff then cutoff_occurred? &lt;- true
 *           else if result != failure then return result
 *       if cutoff_occurred? then return cutoff else return failure
 * </pre>
 *
 * Figure 3.17 A recursive implementation of depth-limited search.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using TreeFrontierExpander.nodeExpander() or
 * GraphFrontierExpander.nodeExpander() as an argument, respectively
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
object depthLimitedSearch {
  def apply[S, A](nodeExpander: NodeExpander[S, A])(limit: Int): Search[S, A, Seq[A]] = problem => {
    def recursiveDLS(node: Node[S, A], limit: Int): SearchResult[Seq[A]] = {
      if (problem.goalTest(node.state))
        Success(solutionActions(node))
      else if (limit == 0)
        Cutoff
      else {
        val children = nodeExpander(problem, node)
        children.foldRight[SearchResult[Seq[A]]](Failure) { (child, result) =>
          recursiveDLS(child, limit - 1) match {
            case success@Success(_) => return success
            case Cutoff => Cutoff
            case Failure => result
          }
        }
      }
    }
    recursiveDLS(Node[S, A](problem.initialState, None, None, 0), limit)
  }
}
