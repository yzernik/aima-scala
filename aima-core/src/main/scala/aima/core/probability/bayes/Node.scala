package aima.core.probability.bayes

import aima.core.probability.RandomVariable
import aima.core.probability.impl.FiniteRandomVariable

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 511.<br>
 * <br>
 * A node is annotated with quantitative probability information. Each node
 * corresponds to a random variable, which may be discrete or continuous. If
 * there is an arrow from node X to node Y in a Bayesian Network, X is said to
 * be a parent of Y and Y is a child of X. Each node X<sub>i</sub> has a
 * conditional probability distribution P(X<sub>i</sub> |
 * Parents(X<sub>i</sub>)) that quantifies the effect of the parents on the
 * node. <br>
 *
 * @author Alex DiCarlo
 */
trait Node[A] {
  /**
   * The RandomVariable this Node is for/on.
   */
  def variable: RandomVariable[A]

  /**
   * True if this Node has no parents.
   */
  def isRoot: Boolean = parents.size == 0

  /**
   * The parent Nodes for this Node.
   */
  def parents: List[Node[_]]

  /**
   * The children Nodes for this Node.
   */
  def children: List[Node[_]]

  /**
   * Get this Node's Markov Blanket:<br>
   * 'A node is conditionally independent of all other nodes in the network,
   * given its parents, children, and children's parents - that is, given its
   * <b>MARKOV BLANKET</b> (AIMA3e pg, 517).
   *
   * @return this Node's Markov Blanket.
   */
  def markovBlanket: List[Node[_]] = parents ::: children ::: children flatMap {_.parents}

  /**
   * The Conditional Probability Distribution associated with this Node.
   */
  def cpd: ConditionalProbabilityDistribution[A]
}

/**
 * A node over a Random Variable that has a countable domain (may be infinite).
 */
trait DiscreteNode[A] extends Node[A]

/**
 * A node over a Random Variable that has a continuous domain.
 */
trait ContinuousNode[A] extends Node[A]

/**
 * A node over a Random Variable that has a finite countable domain.
 */
trait FiniteNode[A] extends DiscreteNode[A] {
  /**
   * The RandomVariable this Node is for/on.
   */
  override def variable: FiniteRandomVariable[A]

  /**
   * The Conditional Probability Table detailing the finite set of probabilities for this Node.
   */
  def cpt: ConditionalProbabilityTable[A]

  lazy val cpd: ConditionalProbabilityDistribution[A] = cpt
}