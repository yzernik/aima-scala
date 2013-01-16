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

package aima.core.probability.bayes

import aima.core.probability.RandomVariable

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 510.<br>
 * <br>
 * Bayesian Networks are used to represent the dependencies among Random
 * Variables. They can represent essentially any full joint probability
 * distribution and in many cases can do so very concisely. A Bayesian network
 * is a directed graph in which each node is annotated with quantitative
 * probability information. The full specification is as follows:<br>
 * <br>
 * 1. Each node corresponds to a random variable, which may be discrete or
 * continuous.<br>
 * <br>
 * 2. A set of directed links or arrows connects pairs of nodes. If there is an
 * arrow from node X to node Y, X is said to be a parent of Y. The graph has no
 * directed cycles (and hence is a directed acyclic graph, or <b>DAG</b>.<br>
 * <br>
 * 3. Each node X<sub>i</sub> has a conditional probability distribution
 * P(X<sub>i</sub> | Parents(X<sub>i</sub>)) that quantifies the effect of the
 * parents on the node.<br>
 * <br>
 * The topology of the network - the set of nodes and links - specifies the
 * conditional independence relationships that hold in the domain.<br>
 * <br>
 * A network with both discrete and continuous variables is called a <b>hybrid
 * Bayesian network</b>.<br>
 * <br>
 * <b>Note(1)</b>: "Bayesian Network" is the most common name used, but there
 * are many synonyms, including "belief network", "probabilistic network",
 * "causal network", and "knowledge map".
 *
 * @author Alex DiCarlo
 */
trait BayesianNetwork {
  /**
   * A list of the RandomVariables, in topological order, contained within the network.
   */
  def variables: List[RandomVariable[_]]

  /**
   *
   * @param variable the RandomVariable whose corresponding Node is to be retrieved.
   * @return the Node associated with the random variable in this Bayesian Network or None if it does not exist
   */
  def nodeFor[A](variable: RandomVariable[A]): Option[Node[A]]
}