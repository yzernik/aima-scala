package aima.core

/**
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
package object agent {
  /**
   * A simple type for a "condition-action rule" matcher
   */
  type RuleMatcher[S, A] = S => Option[A]
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): pg 50.<br>
   *
   * This knowledge about "how the world works" - whether implemented in simple
   * Boolean circuits or in complete scientific theories - is called a model of
   * the world. An Agent that uses such a model is called a model-based agent.
   */
  type Model[S, A, P] = (S, Option[A], P) => S
}
