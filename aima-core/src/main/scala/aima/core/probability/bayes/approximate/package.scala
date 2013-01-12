package aima.core.probability.bayes

import aima.core.probability.impl.{AssignmentProposition}

package object approximate {
  type Event = List[AssignmentProposition]

  private[approximate] def randomSample[A](node: Node[A], event: Event): A =
    node.cpd.sample(util.Random.nextDouble(), event)

  private[approximate] def eventValuesForParents(node: Node[_], event: Event): Event =
    (event filter {case (v, assign) => node.parents.contains(v)})

  /**
   * Get a random sample from <b>P</b>(X<sub>i</sub> | mb(X<sub>i</sub>)),
   * where mb(X<sub>i</sub>) is the Markov Blanket of X<sub>i</sub>. The
   * probability of a variable given its Markov blanket is proportional to the
   * probability of the variable given its parents times the probability of
   * each child given its respective parents (see equation 14.12 pg. 538
   * AIMA3e):<br>
   * <br>
   * P(x'<sub>i</sub>|mb(Xi)) =
   * &alpha;P(x'<sub>i</sub>|parents(X<sub>i</sub>)) *
   * &prod;<sub>Y<sub>j</sub> &isin; Children(X<sub>i</sub>)</sub>
   * P(y<sub>j</sub>|parents(Y<sub>j</sub>))
   *
   * @param node a Node from a Bayesian network for the Random Variable X<sub>i</sub>.
   * @param event comprising assignments for the Markov Blanket X<sub>i</sub>.
   * @return a random sample from <b>P</b>(X<sub>i</sub> | mb(X<sub>i</sub>))
   */

  private[approximate] def markovBlanketRandomSample[A](node: FiniteNode[A], event: Event): A =
    sample(node.variable, util.Random.nextDouble(), event, markovBlanketDistribution(node, event))

  /**
   * Calculate the probability distribution for <b>P</b>(X<sub>i</sub> |
   * mb(X<sub>i</sub>)), where mb(X<sub>i</sub>) is the Markov Blanket of
   * X<sub>i</sub>. The probability of a variable given its Markov blanket is
   * proportional to the probability of the variable given its parents times
   * the probability of each child given its respective parents (see equation
   * 14.12 pg. 538 AIMA3e):<br>
   * <br>
   * P(x'<sub>i</sub>|mb(Xi)) =
   * &alpha;P(x'<sub>i</sub>|parents(X<sub>i</sub>)) *
   * &prod;<sub>Y<sub>j</sub> &isin; Children(X<sub>i</sub>)</sub>
   * P(y<sub>j</sub>|parents(Y<sub>j</sub>))
   *
   * @param node a Node from a Bayesian network for the Random Variable X<sub>i</sub>.
   * @param event comprising assignments for the Markov Blanket X<sub>i</sub>.
   * @return a random sample from <b>P</b>(X<sub>i</sub> | mb(X<sub>i</sub>))
   */
  private[approximate] def markovBlanketDistribution[A](node: FiniteNode[A], event: Event): List[Double] = {
    node.variable.indexedValues.to[List] map {value =>
      val probability = (node.children foldLeft 1.0) {
        case (prob, child: FiniteNode[_]) =>
          prob * child.cpd.value(eventValuesForParents(node, event))
        case _ => throw new
            IllegalArgumentException("markovBlanketDistribution only works for finite nodes with finite children")
      }
      val value =
        node.cpd.value((event find {_.assign.variable == node.variable}).get :: eventValuesForParents(node, event))
      value * probability
    }
  }
}
