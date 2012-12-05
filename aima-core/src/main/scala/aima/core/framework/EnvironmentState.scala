package aima.core.framework

/**
 * An interface used to indicate a possible state of an Environment.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
trait EnvironmentState extends Iterator[EnvironmentState] {
  /**
   * Moves this environment one time step forward
   * @return resulting Environment
   */
  override def next(): EnvironmentState

  /**
   * Returns false if all environment tasks are complete
   * @return false if all environment tasks are complete
   */
  override def hasNext: Boolean

  /**
   * Compares this environment state with another. If they are equal, then any resulting environment states from
   * stepping (calls to #next) will be equal unless the environment is stochastic. If it is stochastic,
   * they will be equal if they use the same seed.
   *
   * @param other environment
   * @return true if deterministic and equal or stochastic and same seed
   */
  override def equals(other: Any): Boolean

  def canEqual(other: Any): Boolean

  override def hashCode: Int
}
