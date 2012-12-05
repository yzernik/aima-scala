package aima.core.search

import aima.core.search.Node.createChildNode

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object GraphFrontierExpander {
  def apply[S, A](): FrontierExpander[S, A] = {
    val myNodeExpander = nodeExpander[S, A]()
    (problem, node, frontier) => frontier ++ myNodeExpander(problem, node)
  }

  def nodeExpander[S, A](): NodeExpander[S, A] = {
    var explored = Set[Node[S, A]]()
    (problem, node) => {
      explored += node
      problem.actions(node.state) map { createChildNode(problem, node, _) } filter
        { child => explored exists { child == _ } }
    }
  }
}
