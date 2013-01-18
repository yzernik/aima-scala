/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
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
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.10, page
 * 49.<br>
 * <br>
 *
 * <pre>
 * function SIMPLE-RELEX-AGENT(percept) returns an action
 *   persistent: rules, a set of condition-action rules
 *
 *   state  <- INTERPRET-INPUT(percept);
 *   rule   <- RULE-MATCH(state, rules);
 *   action <- rule.ACTION;
 *   return action
 * </pre>
 *
 * Figure 2.10 A simple reflex agent. It acts according to a rule whose
 * condition matches the current state, as defined by the percept.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
abstract class SimpleReflexAgent[S, A, P](private val ruleMatcher: RuleMatcher[S, A]) extends Agent[A, P] {
  /**
   * @param percept
   * @return state associated with percept
   */
  def interpretPercept(percept: P): S

  /**
   *
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A] = ruleMatcher(interpretPercept(percept))
}
