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

package aima.core.search

import aima.core.framework.Agent

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.1, page 67.<br>
 * <br>
 *
 * <pre>
 * function SIMPLE-PROBLEM-SOLVING-AGENT(percept) returns an action
 *   persistent: seq, an action sequence, initially empty
 *               state, some description of the current world state
 *               goal, a goal, initially null
 *               problem, a problem formulation
 *
 *   state &lt;- UPDATE-STATE(state, percept)
 *   if seq is empty then
 *     goal    &lt;- FORMULATE-GOAL(state)
 *     problem &lt;- FORMULATE-PROBLEM(state, goal)
 *     seq     &lt;- SEARCH(problem)
 *     if seq = failure then return a null action
 *   action &lt;- FIRST(seq)
 *   seq &lt;- REST(seq)
 *   return action
 * </pre>
 *
 * Figure 3.1 A simple problem-solving agent. It first formulates a goal and a
 * problem, searches for a sequence of actions that would solve the problem, and
 * then executes the actions one at a time. When this is complete, it formulates
 * another goal and starts over.<br>
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
final class SimpleProblemSolvingAgent[S, A, P](
    initialState: S,
    updateState: UpdateState[S, P],
    formulateGoal: FormulateGoal[S],
    formulateProblem: FormulateProblem[S, A],
    search: Search[S, A, Seq[A]]) extends Agent[A, P] {
  private var actions = Seq[A]()
  private var state = initialState

  /**
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A] = {
    state = updateState(state, percept)
    if (actions.isEmpty) {
      actions = search(formulateProblem(state, formulateGoal(state))) match {
        case Success(result) => result
        case _ => Seq()
      }
    }
    val action = actions.headOption
    actions = actions.tail
    action
  }
}
