/*
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

package aima.core.search.online

import aima.core.framework.Agent
import aima.core.search.Heuristic

/**
 * Artificial Intelligence A Modern Approach 3rdd Edition): Figure 4.24, page
 * 152.<br>
 * <br>
 *
 * <pre>
 * function LRTA*-AGENT(s') returns an action
 *   inputs: s', a percept that identifies the current state
 *   persistent: result, a table, indexed by state and action, initially empty
 *               H, a table of cost estimates indexed by state, initially empty
 *               s, a, the previous state and action, initially null
 *
 *   if GOAL-TEST(s') then return stop
 *   if s' is a new state (not in H) then H[s'] &lt;- h(s')
 *   if s is not null
 *     result[s, a] &lt;- s'
 *     H[s] &lt;-        min LRTA*-COST(s, b, result[s, b], H)
 *             b (element of) ACTIONS(s)
 *   a &lt;- an action b in ACTIONS(s') that minimizes LRTA*-COST(s', b, result[s', b], H)
 *   s &lt;- s'
 *   return a
 *
 * function LRTA*-COST(s, a, s', H) returns a cost estimate
 *   if s' is undefined then return h(s)
 *   else return c(s, a, s') + H[s']
 * </pre>
 *
 * Figure 4.24 LRTA*-AGENT selects an action according to the value of
 * neighboring states, which are updated as the agent moves about the state
 * space.<br>
 * <br>
 * <b>Note:</b> This algorithm fails to exit if the goal does not exist (e.g.
 * A<->B Goal=X), this could be an issue with the implementation. Comments
 * welcome.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
final class LRTAStarAgent[S, A, P](
  perceptMapper: PerceptMapper[P, S],
  problem: OnlineSearchProblem[S, A],
  h: Heuristic[S]) extends Agent[A, P] {
  private var result = Map[(S, A), S]()
  private var H = Map[S, Double]()
  private var a: Option[A] = None
  private var s: Option[S] = None

  /**
   *
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A] = {
    def lrtaStarCost(s: S, a: A, sPrime: Option[S]): Double = {
      sPrime map { sPrime => problem.stepCost(s, a, sPrime) + H.get(sPrime).get } getOrElse { h(s) }
    }

    val sPrime = perceptMapper(percept)
    if (problem.goalTest(sPrime))
      return None
    if (!(H isDefinedAt sPrime))
      H += sPrime → h(sPrime)
    s match {
      case Some(s) =>
        result += (s, a.get) → sPrime
        H += s → problem.actions(s).map(b => lrtaStarCost(s, b, result.get((s, b)))).min
      case None =>
    }
    a = Some(problem.actions(sPrime) minBy { b => lrtaStarCost(sPrime, b, result.get((sPrime, b))) })
    s = Some(sPrime)
    a
  }
}
