package aima.core.search

import aima.core.search.Node.createChildNode

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object TreeFrontierExpander {
  def apply[S, A](): FrontierExpander[S, A] =
    (problem, node, frontier) =>
      frontier ++ nodeExpander()(problem, node)

  def nodeExpander[S, A](): NodeExpander[S, A] =
    (problem, node) =>
      problem.actions(node.state) map { createChildNode(problem, node, _) }
}
