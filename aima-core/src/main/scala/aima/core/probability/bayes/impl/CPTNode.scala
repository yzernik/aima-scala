package aima.core.probability.bayes.impl

import aima.core.probability.bayes.FiniteNode
import aima.core.probability.impl.FiniteRandomVariable

/**
 * Default implementation of the FiniteNode interface that uses a fully
 * specified Conditional Probability Table to represent the Node's conditional
 * distribution.
 *
 * @author Alex DiCarlo
 */
final class CPTNode[A] private (
  val variable: FiniteRandomVariable[A],
  val distribution: List[Double],
  _parents: => List[CPTNode[_]],
  _children: => List[CPTNode[_]]) extends FiniteNode[A] {
  lazy val parents: List[CPTNode[_]] = _parents
  lazy val children: List[CPTNode[_]] = _children
  lazy val cpt: CPT[A] = new CPT(variable, distribution, parents map {_.variable})

  override def toString: String = {
    def names(nodes: List[CPTNode[_]]): String =
      nodes map {child => child.variable.name} reduceOption {_ + ", " + "" + _} match {
        case Some(str) => str
        case None => ""
      }

    def nodesString(nodes: List[CPTNode[_]]): String = nodes map {node =>
      s"CPTNode(${node.variable}, ${node.distribution}, ParentNames(${names(node.parents)}), " +
        s"ChildrenNames(${names(node.children)})"
    } reduceOption {_ + ", " + _} match {
      case Some(str) => s"List($str)"
      case None => "List()"
    }

    s"CPTNode($variable, $distribution, ${nodesString(parents)}, ${nodesString(children)})"
  }
}

object CPTNode {
  def apply[A](
    variable: FiniteRandomVariable[A],
    distribution: List[Double],
    parents: List[CPTNode[_]],
    _children: => List[CPTNode[_]]): CPTNode[A] = new CPTNode(variable, distribution, parents, _children)

  private type Unapply[A] = Option[(FiniteRandomVariable[A], List[Double], List[CPTNode[_]], List[CPTNode[_]])]
  def unapply[A](node: CPTNode[A]): Unapply[A] = Some(node.variable, node.distribution, node.parents, node.children)
}