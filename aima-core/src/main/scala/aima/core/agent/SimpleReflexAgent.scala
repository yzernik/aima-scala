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
