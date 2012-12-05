package aima.core.framework

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.1, page 35.<br>
 *
 * Figure 2.1 Agents interact with environments through sensors and actuators.
 *
 * @tparam A Action
 * @tparam P Percept
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
trait Agent[A, P] {
  /**
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A]
}
