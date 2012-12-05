package aima.core.search

import scala.annotation.tailrec
import scala.Some

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object search {
  def apply[S, A](frontier: Frontier[S, A], frontierExpander: FrontierExpander[S, A]): Search[S, A, Seq[A]] =
    problem => {
      @tailrec
      def recur(frontier: Frontier[S, A]): SearchResult[Seq[A]] = frontier.headOption match {
        case None => Failure
        case Some(node) if problem.goalTest(node.state) => Success(solutionActions(node))
        case Some(node) => recur(frontierExpander(problem, node, frontier))
      }
      recur(frontier.take(0) ++ Traversable(Node[S, A](problem.initialState, None, None, 0)))
    }
}
