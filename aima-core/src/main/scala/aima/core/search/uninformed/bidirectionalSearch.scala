package aima.core.search.uninformed

import aima.core.search._
import aima.core.search.Node.createChildNode
import scala.util.control.TailCalls._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 90.<br>
 * <br>
 * Bidirectional search.<br>
 * <br>
 * <b>Note:</b> Based on the description of this algorithm i.e. 'Bidirectional
 * search is implemented by replacing the goal test with a check to see whether
 * the frontiers of the two searches intersect;', it is possible for the
 * searches to pass each other's frontiers by, in particular if the problem is
 * not fully reversible (i.e. unidirectional links on a graph), and could
 * instead intersect at the explored set.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object bidirectionalSearch {
  /**
   * An interface describing a problem that can be tackled from both directions at
   * once (i.e InitialState<->Goal).
   */
  trait BidirectionalProblem[S, A] {
    val originalProblem: Problem[S, A]
    val reverseProblem: Problem[S, A]
  }

  def apply[S, A](
    opFrontier: Frontier[S, A],
    rpFrontier: Frontier[S, A],
    problem: BidirectionalProblem[S, A]): SearchResult[Seq[A]] = {
    val originalProblem = problem.originalProblem
    val reverseProblem = problem.reverseProblem

    def expandFrontier(problem: Problem[S, A], node: Node[S, A], frontier: Frontier[S, A], explored: Explored[S, A]) = {
      val children = problem.actions(node.state) map { a => createChildNode(problem, node, a) }
      children.foldRight(frontier.tail) {
        case (child, f) if f.exists(_ == child) || explored.exists(_ == child) || node == child => f
        case (child, f) => f ++ Traversable(child)
      }
    }

    def possibleSolution(opNode: Option[Node[S, A]], rpNode: Option[Node[S, A]]): Option[Seq[A]] = {
      def getActions(oNode: Option[Node[S, A]]): Seq[A] =
        oNode.map(solutionActions(_)).getOrElse(Seq())
      val possiblePath = getActions(opNode) ++ getActions(rpNode).reverse
      possiblePath.foldLeft(originalProblem.initialState) { (currState, action) =>
        if (originalProblem.actions(currState).exists(_ == action))
          originalProblem.result(currState, action)
        else
          return None
      }
      Some(possiblePath)
    }

    def recur(
      opFrontier: Frontier[S, A],
      rpFrontier: Frontier[S, A],
      opExplored: Explored[S, A],
      rpExplored: Explored[S, A]): TailRec[SearchResult[Seq[A]]] = {
      val newOpFrontier = opFrontier.headOption map
        { expandFrontier(originalProblem, _, opFrontier, opExplored) } getOrElse opFrontier
      val newOpExplored = opFrontier.headOption map { opExplored + _ } getOrElse opExplored
      val newRpFrontier = rpFrontier.headOption map
        { expandFrontier(reverseProblem, _, rpFrontier, rpExplored) } getOrElse rpFrontier
      val newRpExplored = rpFrontier.headOption map { rpExplored + _ } getOrElse rpExplored

      @inline
      def testSolution(popNode: Option[Node[S, A]], prpNode: Option[Node[S, A]]): TailRec[SearchResult[Seq[A]]] =
        possibleSolution(popNode, prpNode) match {
          case Some(solution) => done(Success(solution))
          case _ => tailcall(recur(newOpFrontier, newRpFrontier, newOpExplored, newRpExplored))
        }

      (opFrontier.headOption, rpFrontier.headOption) match {
        case (Some(opNode), Some(rpNode)) if rpFrontier.exists(_ == opNode) =>
          testSolution(Some(opNode), rpFrontier.find(_ == opNode))
        case (Some(opNode), Some(rpNode)) if opFrontier.exists(_ == rpNode) =>
          testSolution(opFrontier.find(_ == rpNode), Some(rpNode))
        case (Some(opNode), Some(rpNode)) if opNode.state == rpNode.state =>
          testSolution(Some(opNode), Some(rpNode))
        case (Some(opNode), _) if originalProblem.goalTest(opNode.state) =>
          testSolution(Some(opNode), None)
        case (_, Some(rpNode)) if rpNode.state == originalProblem.initialState =>
          testSolution(None, Some(rpNode))
        case (None, None) =>
          done(Failure)
        case _ =>
          tailcall(recur(newOpFrontier, newRpFrontier, newOpExplored, newRpExplored))
      }

    }
    val startOpNode = Node[S, A](originalProblem.initialState, None, None, 0)
    val startRpNode = Node[S, A](reverseProblem.initialState, None, None, 0)
    recur(opFrontier ++ Traversable(startOpNode), rpFrontier ++ Traversable(startRpNode), Set(), Set()).result
  }
}
