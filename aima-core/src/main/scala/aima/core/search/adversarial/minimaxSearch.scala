package aima.core.search.adversarial

import aima.core.search._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 169.<br>
 *
 * <pre>
 * <code>
 * function MINIMAX-DECISION(state) returns an action
 *   return argmax_[a in ACTIONS(s)] MIN-VALUE(RESULT(state, a))
 *
 * function MAX-VALUE(state) returns a utility value
 *   if TERMINAL-TEST(state) then return UTILITY(state)
 *   v = -infinity
 *   for each a in ACTIONS(state) do
 *     v = MAX(v, MIN-VALUE(RESULT(s, a)))
 *   return v
 *
 * function MIN-VALUE(state) returns a utility value
 *   if TERMINAL-TEST(state) then return UTILITY(state)
 *     v = infinity
 *     for each a in ACTIONS(state) do
 *       v  = MIN(v, MAX-VALUE(RESULT(s, a)))
 *   return v
 * </code>
 * </pre>
 *
 * Figure 5.3 An algorithm for calculating minimax decisions. It returns the
 * action corresponding to the best possible move, that is, the move that leads
 * to the outcome with the best utility, under the assumption that the opponent
 * plays to minimize utility. The functions MAX-VALUE and MIN-VALUE go through
 * the whole game tree, all the way to the leaves, to determine the backed-up
 * value of a state. The notation argmax_[a in S] f(a) computes the element a of
 * set S that has the maximum value of f(a).
 *
 * <b>Note:</b> Supports using a cutoff and eval function. If the entire tree should be searched,
 * simply use problem.terminalTest for cutoff and problem.utility for eval
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
object minimaxSearch {
  def apply[S, A](problem: AdversarialProblem[S, A], cutoff: Cutoff[S], eval: Heuristic[S]): AdversarialSearch[S, A] = {
    def maxValue(state: S, d: Int): Double =
      if (cutoff(state, d)) eval(state)
      else problem.actions(state).map(a => minValue(problem.result(state, a), d + 1)).max

    def minValue(state: S, d: Int): Double =
      if (cutoff(state, d)) eval(state)
      else problem.actions(state).map(a => maxValue(problem.result(state, a), d + 1)).min

    state => {
      Success(problem.actions(state) maxBy { a => minValue(problem.result(state, a), 1) })
    }
  }
}
