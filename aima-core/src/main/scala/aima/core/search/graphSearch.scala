package aima.core.search

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.7, page 77. <br>
 * <br>
 *
 * <pre>
 * function GRAPH-SEARCH(problem) returns a solution, or failure
 *   initialize the frontier using the initial state of problem
 *   initialize the explored set to be empty
 *   loop do
 *     if the frontier is empty then return failure
 *     choose a leaf node and remove it from the frontier
 *     if the node contains a goal state then return the corresponding solution
 *     add the node to the explored set
 *     expand the chosen node, adding the resulting nodes to the frontier
 *       only if not in the frontier or explored set
 * </pre>
 *
 * Figure 3.7 An informal description of the general graph-search algorithm.
 *
 * Author: dicarlo2 (Alex)
 * Date: 11/19/12
 */
object graphSearch {
  def apply[S, A](frontier: Frontier[S, A]): Search[S, A, Seq[A]] =
    search(frontier, GraphFrontierExpander())
}
