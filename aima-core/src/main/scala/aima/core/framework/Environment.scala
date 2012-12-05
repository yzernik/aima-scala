package aima.core.framework

/**
 * An abstract description of possible discrete Environments in which Agent(s)
 * can perceive and act.
 *
 * @tparam A Action
 * @tparam P Percept
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
abstract class Environment[A, P](startState: EnvironmentState) {
  /**
   * Measures performance of agent in current environment
   * @param agent to be measured
   * @return value where agents with better performance have higher value
   */
  def measurePerformance(agent: Agent[A, P]): Double

  override def equals(other: Any): Boolean

  def canEqual(other: Any): Boolean

  override def hashCode: Int
}
