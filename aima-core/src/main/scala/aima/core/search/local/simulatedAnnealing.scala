package aima.core.search.local

import aima.core.search._
import scala.util.Random
import scala.annotation.tailrec
import java.lang.Math.exp

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.5, page
 * 126.<br>
 * <br>
 *
 * <pre>
 * function SIMULATED-ANNEALING(problem, schedule) returns a solution state
 *
 *   current &lt;- MAKE-NODE(problem.INITIAL-STATE)
 *   for t = 1 to INFINITY do
 *     T &lt;- schedule(t)
 *     if T = 0 then return current
 *     next &lt;- a randomly selected successor of current
 *     /\E &lt;- next.VALUE - current.value
 *     if /\E &gt; 0 then current &lt;- next
 *     else current &lt;- next only with probability e&circ;(/\E/T)
 * </pre>
 *
 * Figure 4.5 The simulated annealing search algorithm, a version of stochastic
 * hill climbing where some downhill moves are allowed. Downhill moves are
 * accepted readily early in the annealing schedule and then less often as time
 * goes on. The schedule input determines the value of the temperature T as a
 * function of time.
 *
 * <b>Note:</b> Supports both Tree and Graph based versions by using GraphFrontierExpander.nodeExpander or
 * TreeFrontierExpander.nodeExpander as an argument. Note that using the graph expander will not allow revisiting of
 * previous states, which in some ways is contrary to the spirit of this algorithm
 *
 * <b>Note:</b> Returns a tuple of (local maximum state, sequence of actions from start state to local maximum state)
 * . Notice that the sequence of actions is not necessarily optimal, however an optimal sequence can be found using
 * any of the algorithms in uninformed or informed search with the local maximum as the goal state.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
object simulatedAnnealing {
  type Schedule = Int => Int

  def apply[S, A](nodeExpander: NodeExpander[S, A])
    (problem: Problem[S, A], h: Heuristic[S], schedule: Schedule): Search[S, A, (S, Seq[A])] = problem => {
    val rand = new Random()
    def shuffleNeighbors = (node: Node[S, A]) => rand.shuffle(nodeExpander(problem, node))
    @tailrec
    def recur(times: Stream[Int], current: Node[S, A], shuffled: Traversable[Node[S, A]]): SearchResult[(S, Seq[A])] = {
      val T = schedule(times.head)
      if (T == 0) return Success(current.state, solutionActions(current))
      shuffled.headOption match {
        case Some(neighbor) =>
          val deltaE = h(neighbor.state) - h(current.state)
          val next = if (deltaE > 0 || rand.nextDouble() <= exp(deltaE / T)) neighbor else current
          recur(times.tail, next, shuffleNeighbors(next))
        case None => Failure
      }
    }
    val startNode = Node[S, A](problem.initialState, None, None, 0)
    recur(Stream.from(0), startNode, shuffleNeighbors(startNode))
  }
}
