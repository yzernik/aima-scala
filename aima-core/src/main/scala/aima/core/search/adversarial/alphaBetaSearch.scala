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

package aima.core.search.adversarial

import aima.core.search._
import java.lang.Math.{max, min}

/**
 * Artificial Intelligence A Modern Approach (3rd Ed.): Page 173.<br>
 *
 * <pre>
 * <code>
 * function ALPHA-BETA-SEARCH(state) returns an action
 *   v = MAX-VALUE(state, -infinity, +infinity)
 *   return the action in ACTIONS(state) with value v
 *
 * function MAX-VALUE(state, alpha, beta) returns a utility value
 *   if TERMINAL-TEST(state) then return UTILITY(state)
 *   v = -infinity
 *   for each a in ACTIONS(state) do
 *     v = MAX(v, MIN-VALUE(RESULT(s, a), alpha, beta))
 *     if v >= beta then return v
 *     alpha = MAX(alpha, v)
 *   return v
 *
 * function MIN-VALUE(state, alpha, beta) returns a utility value
 *   if TERMINAL-TEST(state) then return UTILITY(state)
 *   v = infinity
 *   for each a in ACTIONS(state) do
 *     v = MIN(v, MAX-VALUE(RESULT(s,a), alpha, beta))
 *     if v <= alpha then return v
 *     beta = MIN(beta, v)
 *   return v
 * </code>
 * </pre>
 *
 * Figure 5.7 The alpha-beta search algorithm. Notice that these routines are
 * the same as the MINIMAX functions in Figure 5.3, except for the two lines in
 * each of MIN-VALUE and MAX-VALUE that maintain alpha and beta (and the
 * bookkeeping to pass these parameters along).
 *
 * <b>Note:</b> Supports using a cutoff and eval function. If the entire tree should be searched,
 * simply use problem.terminalTest for cutoff and problem.utility for eval
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
object alphaBetaSearch {
  def apply[S, A](problem: AdversarialProblem[S, A], cutoff: Cutoff[S], eval: Heuristic[S]): AdversarialSearch[S, A] = {
    def maxValue(state: S, α: Double, β: Double, d: Int): Double =
      if (cutoff(state, d)) eval(state)
      else problem.actions(state).foldLeft(Double.MinValue) { (v, action) =>
        if (v >= β) return v
        else max(v, minValue(problem.result(state, action), max(α, v), β, d + 1))
      }

    def minValue(state: S, α: Double, β: Double, d: Int): Double =
      if (cutoff(state, d)) eval(state)
      else problem.actions(state).foldLeft(Double.MaxValue) { (v, action) =>
        if (v <= α) return v
        else min(v, maxValue(problem.result(state, action), α, min(β, v), d + 1))
      }

    state => {
      val (maxAction, _) = problem.actions(state) map { action =>
        (action, minValue(problem.result(state, action), Double.MinValue, Double.MaxValue, 1)) } maxBy { _._2 }
      Success(maxAction)
    }
  }
}
