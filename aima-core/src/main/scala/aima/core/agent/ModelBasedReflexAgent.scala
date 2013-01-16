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

package aima.core.agent

import aima.core.framework._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.12, page
 * 51.<br>
 * <br>
 *
 * <pre>
 * function MODEL-BASED-REFLEX-AGENT(percept) returns an action
 *   persistent: state, the agent's current conception of the world state
 *               model, a description of how the next state depends on current state and action
 *               rules, a set of condition-action rules
 *               action, the most recent action, initially none
 *
 *   state  <- UPDATE-STATE(state, action, percept, model)
 *   rule   <- RULE-MATCH(state, rules)
 *   action <- rule.ACTION
 *   return action
 * </pre>
 *
 * Figure 2.12 A model-based reflex agent. It keeps track of the current state
 * of the world using an internal model. It then chooses an action in the same
 * way as the reflex agent.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
final class ModelBasedReflexAgent[S, A, P](
  startState: S,
  model: Model[S, A, P],
  ruleMatcher: RuleMatcher[S, A]) extends Agent[A, P] {
  private var action: Option[A] = None
  private var state: S = startState

  /**
   *
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A] = {
    state = model(state, action, percept)
    action = ruleMatcher(state)
    action
  }
}
